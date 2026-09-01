/*
 * dbc_reader.c -- stream DATASUS DBC records directly into R columns
 *
 * The DBC header is the original DBF header. Only the record area is
 * compressed with PKWare DCL implode. blast() emits that area in chunks;
 * this reader assembles one fixed-width DBF record at a time and populates
 * the result without materialising an intermediate DBF file.
 */

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Utils.h>

#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "blast.h"
#include "dbc_crc32.h"

#define DBC_INPUT_BUFFER_SIZE 16384u
#define DBC_CRC_SIZE 4L
#define DBF_MIN_HEADER_SIZE 33u
#define DBF_FIELD_DESCRIPTOR_SIZE 32u
#define DBF_MAX_FIELD_WIDTH 255u
#define DBC_ERROR_SIZE 256u
#define DBC_INTERRUPT_BYTES (8u * 1024u * 1024u)

typedef enum {
    DBF_COLUMN_STRING = 1,
    DBF_COLUMN_NUMBER = 2,
    DBF_COLUMN_LOGICAL = 3
} dbf_column_kind;

typedef struct {
    char name[12];
    char type;
    unsigned int width;
    unsigned int decimals;
    unsigned int offset;
    dbf_column_kind kind;
    int integer_candidate;
    int selected;
    int output_index;
    SEXP column;
} dbf_field;

typedef struct {
    FILE *file;
    unsigned char buffer[DBC_INPUT_BUFFER_SIZE];
    struct dbc_reader_tag *reader;
} dbc_input;

typedef struct dbc_reader_tag {
    const char *path;
    FILE *file;
    unsigned char *header;
    unsigned char *record;
    dbf_field *fields;
    size_t header_size;
    unsigned int record_size;
    unsigned int record_used;
    unsigned int field_count;
    unsigned int selected_count;
    uint32_t record_count;
    uint32_t row;
    unsigned int invalid_logicals;
    unsigned int invalid_numerics;
    unsigned int first_invalid_numeric_field;
    uint32_t first_invalid_numeric_row;
    unsigned int trailing_count;
    size_t bytes_since_interrupt;
    uint32_t expected_checksum;
    dbc_crc32_state checksum;
    int callback_failed;
    char error[DBC_ERROR_SIZE];
    SEXP data_frame;
    SEXP data_types;
    SEXP selection;
} dbc_reader;

static uint16_t read_le16(const unsigned char *value)
{
    return (uint16_t)value[0] | ((uint16_t)value[1] << 8);
}

static uint32_t read_le32(const unsigned char *value)
{
    return (uint32_t)value[0] |
           ((uint32_t)value[1] << 8) |
           ((uint32_t)value[2] << 16) |
           ((uint32_t)value[3] << 24);
}

static int valid_dbf_version(unsigned char version)
{
    switch (version) {
    case 0x02:
    case 0x03:
    case 0x04:
    case 0x05:
    case 0x30:
    case 0x31:
    case 0x32:
    case 0x43:
    case 0x63:
    case 0x7b:
    case 0x83:
    case 0x8b:
    case 0x8e:
    case 0xcb:
    case 0xf5:
        return 1;
    default:
        return 0;
    }
}

static void set_reader_error(dbc_reader *reader, const char *message)
{
    if (reader->error[0] == '\0') {
        snprintf(reader->error, sizeof(reader->error), "%s", message);
    }
    reader->callback_failed = 1;
}

static unsigned int trimmed_span(
    const unsigned char *value,
    unsigned int width,
    unsigned int *start
)
{
    unsigned int first = 0;
    unsigned int last = width;
    unsigned int index;

    while (first < last && value[first] == ' ') {
        first++;
    }
    while (last > first && value[last - 1] == ' ') {
        last--;
    }
    for (index = first; index < last; index++) {
        if (value[index] == '\0') {
            last = index;
            break;
        }
    }

    *start = first;
    return last - first;
}

static int all_zero_date(const unsigned char *value, unsigned int length)
{
    unsigned int index;

    if (length < 8u) {
        return 0;
    }
    for (index = 0; index < 8u; index++) {
        if (value[index] != '0') {
            return 0;
        }
    }
    return 1;
}

static int all_bytes_equal(
    const unsigned char *value,
    unsigned int length,
    unsigned char expected
)
{
    unsigned int index;

    for (index = 0u; index < length; index++) {
        if (value[index] != expected) {
            return 0;
        }
    }
    return 1;
}

static int valid_numeric_byte(unsigned char value)
{
    return (value >= '0' && value <= '9') || value == '+' || value == '-' ||
        value == '.' || value == 'e' || value == 'E';
}

static void record_invalid_numeric(
    dbc_reader *reader,
    unsigned int field_index
)
{
    if (reader->invalid_numerics == 0u) {
        reader->first_invalid_numeric_field = field_index + 1u;
        reader->first_invalid_numeric_row = reader->row + 1u;
    }
    reader->invalid_numerics++;
}

static void set_numeric_precision_error(
    dbc_reader *reader,
    unsigned int field_index
)
{
    if (reader->error[0] == '\0') {
        snprintf(
            reader->error,
            sizeof(reader->error),
            "Numeric value in DBF field %u at record %u cannot be "
            "represented exactly as an R double",
            field_index + 1u,
            reader->row + 1u
        );
    }
    reader->callback_failed = 1;
}

/* Return false only for a plain decimal integer changed by conversion to
 * double. Other valid DBF numeric forms retain ordinary floating semantics. */
static int exact_plain_integer(
    const unsigned char *value,
    unsigned int length,
    double number
)
{
    char formatted[DBF_MAX_FIELD_WIDTH + 32u];
    unsigned int position = 0u;
    unsigned int digits;
    unsigned int normalized_length;
    int negative = 0;
    int written;

    if (value[position] == '+' || value[position] == '-') {
        negative = value[position] == '-';
        position++;
    }
    if (position == length) {
        return 1;
    }
    for (digits = position; digits < length; digits++) {
        if (value[digits] < '0' || value[digits] > '9') {
            return 1;
        }
    }
    while (position + 1u < length && value[position] == '0') {
        position++;
    }

    written = snprintf(formatted, sizeof(formatted), "%.0f", number);
    if (written < 0 || (size_t)written >= sizeof(formatted)) {
        return 0;
    }
    normalized_length = length - position + (negative ? 1u : 0u);
    if ((unsigned int)written != normalized_length) {
        return 0;
    }
    if (negative) {
        return formatted[0] == '-' &&
            memcmp(formatted + 1, value + position, length - position) == 0;
    }
    return memcmp(formatted, value + position, length - position) == 0;
}

static void validate_dbf_field(
    char type,
    unsigned int width,
    unsigned int decimals,
    unsigned int name_length,
    unsigned int offset,
    unsigned int record_size
)
{
    if (name_length == 0u) {
        error("DBF field has an empty name.");
    }
    if (type != 'C' && type != 'D' && type != 'F' &&
        type != 'L' && type != 'N') {
        error(
            "Unsupported DBF field type 0x%02x.",
            (unsigned int)(unsigned char)type
        );
    }
    if (width == 0u || offset > record_size ||
        width > record_size - offset) {
        error("DBF field layout exceeds the declared record size.");
    }
    if (type == 'D' && width != 8u) {
        error("DBF date fields must have width 8.");
    }
    if (type == 'L' && width != 1u) {
        error("DBF logical fields must have width 1.");
    }
    if ((type == 'N' || type == 'F') && decimals > 0u &&
        decimals + 1u >= width) {
        error("Invalid decimal count in DBF numeric field.");
    }
}

static int parse_dbf_record(dbc_reader *reader)
{
    unsigned int index;
    R_xlen_t row = (R_xlen_t)reader->row;

    if (reader->record[0] != ' ' && reader->record[0] != '*') {
        set_reader_error(reader, "Invalid DBF record status marker");
        return 1;
    }

    for (index = 0; index < reader->field_count; index++) {
        dbf_field *field = reader->fields + index;

        if (!field->selected) {
            continue;
        }

        const unsigned char *raw = reader->record + field->offset;
        unsigned int start = 0;
        unsigned int length = trimmed_span(raw, field->width, &start);
        const unsigned char *value = raw + start;

        if (field->kind == DBF_COLUMN_STRING) {
            if (length == 0u ||
                (field->type == 'D' && all_zero_date(value, length))) {
                SET_STRING_ELT(field->column, row, NA_STRING);
            } else {
                SET_STRING_ELT(
                    field->column,
                    row,
                    mkCharLenCE((const char *)value, (int)length, CE_BYTES)
                );
            }
        } else if (field->kind == DBF_COLUMN_LOGICAL) {
            int logical_value = NA_LOGICAL;

            if (length > 0u) {
                switch (value[0]) {
                case 'f':
                case 'F':
                case 'n':
                case 'N':
                    logical_value = 0;
                    break;
                case 't':
                case 'T':
                case 'y':
                case 'Y':
                    logical_value = 1;
                    break;
                case '?':
                    logical_value = NA_LOGICAL;
                    break;
                default:
                    reader->invalid_logicals++;
                    break;
                }
            }
            LOGICAL(field->column)[row] = logical_value;
        } else {
            double number = NA_REAL;

            if (length > 0u &&
                !all_bytes_equal(value, length, (unsigned char)'*')) {
                char buffer[DBF_MAX_FIELD_WIDTH + 1u];
                char *end = NULL;
                unsigned int byte_index;
                int valid = 1;

                if (length > DBF_MAX_FIELD_WIDTH) {
                    set_reader_error(reader, "DBF numeric field is too wide");
                    return 1;
                }
                for (byte_index = 0u; byte_index < length; byte_index++) {
                    if (!valid_numeric_byte(value[byte_index])) {
                        valid = 0;
                        break;
                    }
                }
                if (valid) {
                    memcpy(buffer, value, length);
                    buffer[length] = '\0';
                    number = R_strtod(buffer, &end);
                    if (end == buffer || end != buffer + length ||
                        !R_FINITE(number)) {
                        valid = 0;
                    }
                }
                if (!valid) {
                    record_invalid_numeric(reader, index);
                    number = NA_REAL;
                } else if (field->type == 'N' && field->decimals == 0u &&
                    !exact_plain_integer(value, length, number)) {
                    set_numeric_precision_error(reader, index);
                    return 1;
                }
            }

            REAL(field->column)[row] = number;
            if (field->integer_candidate && !ISNA(number)) {
                if (!R_FINITE(number) || number > (double)INT_MAX ||
                    number < (double)(INT_MIN + 1) ||
                    number != (double)((int)number)) {
                    field->integer_candidate = 0;
                }
            }
        }
    }

    return 0;
}

static unsigned dbc_input_callback(void *context, unsigned char **buffer)
{
    dbc_input *input = (dbc_input *)context;
    size_t count;

    *buffer = input->buffer;
    count = fread(
        input->buffer,
        1,
        sizeof(input->buffer),
        input->file
    );
    if (count == 0u && ferror(input->file)) {
        set_reader_error(input->reader, "Failed to read compressed DBC data");
    }
    return (unsigned)count;
}

static int dbc_output_callback(void *context, unsigned char *buffer, unsigned len)
{
    dbc_reader *reader = (dbc_reader *)context;
    unsigned int position = 0;

    dbc_crc32_update(&reader->checksum, buffer, (size_t)len);

    while (position < len) {
        unsigned int available;
        unsigned int remaining;
        unsigned int take;

        /* A conventional DBF may end in 0x1a. Once all declared records have
         * been read, accept that marker but reject every other extra byte. */
        if (reader->row >= reader->record_count) {
            while (position < len) {
                if (reader->trailing_count == 0u && buffer[position] == 0x1a) {
                    reader->trailing_count++;
                    position++;
                } else {
                    set_reader_error(
                        reader,
                        "Unexpected data after the declared DBF records"
                    );
                    return 1;
                }
            }
            return 0;
        }

        available = reader->record_size - reader->record_used;
        remaining = len - position;
        take = remaining < available ? remaining : available;
        memcpy(reader->record + reader->record_used, buffer + position, take);
        reader->record_used += take;
        position += take;
        reader->bytes_since_interrupt += take;
        if (reader->bytes_since_interrupt >= DBC_INTERRUPT_BYTES) {
            R_CheckUserInterrupt();
            reader->bytes_since_interrupt = 0u;
        }

        if (reader->record_used == reader->record_size) {
            if (parse_dbf_record(reader) != 0) {
                return 1;
            }
            reader->row++;
            reader->record_used = 0;

            if ((reader->row & 0xffffu) == 0u) {
                R_CheckUserInterrupt();
            }
        }
    }

    return 0;
}

static void reader_cleanup(void *context, Rboolean jump)
{
    dbc_reader *reader = (dbc_reader *)context;

    (void)jump;
    if (reader->file != NULL) {
        fclose(reader->file);
        reader->file = NULL;
    }
    free(reader->header);
    reader->header = NULL;
    free(reader->record);
    reader->record = NULL;
    free(reader->fields);
    reader->fields = NULL;
}

static void read_header(dbc_reader *reader)
{
    unsigned char size_bytes[2];
    size_t count;

    if (fseek(reader->file, 8L, SEEK_SET) != 0) {
        error("Failed to seek to the DBC header size.");
    }
    count = fread(size_bytes, 1, sizeof(size_bytes), reader->file);
    if (count != sizeof(size_bytes)) {
        error("Failed to read the DBC header size.");
    }

    reader->header_size = (size_t)read_le16(size_bytes);
    if (reader->header_size < DBF_MIN_HEADER_SIZE) {
        error("Invalid DBF header size in DBC file.");
    }

    reader->header = (unsigned char *)malloc(reader->header_size);
    if (reader->header == NULL) {
        error("Could not allocate memory for the DBC header.");
    }
    if (fseek(reader->file, 0L, SEEK_SET) != 0) {
        error("Failed to rewind the DBC file.");
    }
    count = fread(reader->header, 1, reader->header_size, reader->file);
    if (count != reader->header_size) {
        error("Failed to read the complete DBF header from the DBC file.");
    }
}

static void validate_header_structure(dbc_reader *reader)
{
    unsigned int descriptor_bytes;

    if (!valid_dbf_version(reader->header[0])) {
        error("Unsupported or invalid DBF version in DBC file.");
    }

    /* Locate the terminator at a descriptor boundary. Official DataSUS files
     * include conventional headers, one-byte NUL padding, Visual FoxPro's
     * 263-byte backlink area, and a variant with a final NUL instead of 0x0d. */
    reader->field_count = 0u;
    for (descriptor_bytes = DBF_MIN_HEADER_SIZE - 1u +
             DBF_FIELD_DESCRIPTOR_SIZE;
         descriptor_bytes < reader->header_size;
         descriptor_bytes += DBF_FIELD_DESCRIPTOR_SIZE) {
        if (reader->header[descriptor_bytes] == 0x0d) {
            size_t extension = reader->header_size - descriptor_bytes - 1u;

            reader->field_count =
                (descriptor_bytes - (DBF_MIN_HEADER_SIZE - 1u)) /
                DBF_FIELD_DESCRIPTOR_SIZE;
            if (extension == 0u) {
                break;
            }
            if (extension == 1u &&
                reader->header[descriptor_bytes + 1u] == 0x00) {
                break;
            }
            if (extension == 263u &&
                (reader->header[0] == 0x30 ||
                 reader->header[0] == 0x31 ||
                 reader->header[0] == 0x32)) {
                break;
            }
            error("Unsupported DBF header extension in DBC file.");
        }
    }
    if (reader->field_count == 0u) {
        descriptor_bytes = (unsigned int)reader->header_size -
            DBF_MIN_HEADER_SIZE;
        if (reader->header[reader->header_size - 1u] != 0x00 ||
            descriptor_bytes % DBF_FIELD_DESCRIPTOR_SIZE != 0u) {
            error("Invalid DBF field descriptor terminator in DBC file.");
        }
        reader->field_count = descriptor_bytes / DBF_FIELD_DESCRIPTOR_SIZE;
    }
    if (reader->field_count == 0u) {
        error("No DBF fields found in DBC file.");
    }

    reader->record_count = read_le32(reader->header + 4);
    reader->record_size = (unsigned int)read_le16(reader->header + 10);
    if (reader->record_size == 0u) {
        error("Invalid zero-length DBF record in DBC file.");
    }
    if (reader->record_count > (uint32_t)INT_MAX) {
        error("DBC file has more rows than an R data frame can represent.");
    }
}

/* Leaves data_frame protected for the duration of read_dbc_body(). */
static void parse_header(dbc_reader *reader)
{
    unsigned int index;
    unsigned int running_offset = 1u;
    SEXP names;
    SEXP header_snapshot;

    validate_header_structure(reader);

    reader->fields = (dbf_field *)calloc(
        reader->field_count,
        sizeof(dbf_field)
    );
    if (reader->fields == NULL) {
        error("Could not allocate DBF field metadata.");
    }
    reader->record = (unsigned char *)malloc(reader->record_size);
    if (reader->record == NULL) {
        error("Could not allocate the DBF record buffer.");
    }

    if (reader->selection == R_NilValue) {
        reader->selected_count = reader->field_count;
        for (index = 0; index < reader->field_count; index++) {
            reader->fields[index].selected = 1;
            reader->fields[index].output_index = (int)index;
        }
    } else {
        R_xlen_t selection_index;

        if (TYPEOF(reader->selection) != INTSXP) {
            error("column selection must be an integer vector");
        }
        if (XLENGTH(reader->selection) > (R_xlen_t)UINT_MAX) {
            error("too many selected DBF fields");
        }
        reader->selected_count = (unsigned int)XLENGTH(reader->selection);
        for (selection_index = 0;
             selection_index < XLENGTH(reader->selection);
             selection_index++) {
            int field_index = INTEGER(reader->selection)[selection_index];

            if (field_index == NA_INTEGER || field_index < 1 ||
                (unsigned int)field_index > reader->field_count) {
                error("column selection contains an invalid field index");
            }
            field_index--;
            if (reader->fields[field_index].selected) {
                error("column selection contains a duplicated field index");
            }
            reader->fields[field_index].selected = 1;
            reader->fields[field_index].output_index = (int)selection_index;
        }
    }

    reader->data_frame = PROTECT(allocVector(VECSXP, reader->selected_count));
    names = PROTECT(allocVector(STRSXP, reader->selected_count));
    reader->data_types = PROTECT(allocVector(STRSXP, reader->selected_count));

    for (index = 0; index < reader->field_count; index++) {
        const unsigned char *descriptor =
            reader->header + 32u + index * DBF_FIELD_DESCRIPTOR_SIZE;
        dbf_field *field = reader->fields + index;
        unsigned int name_length = 0u;
        SEXP column;
        char type_text[2];

        while (name_length < 11u && descriptor[name_length] != '\0') {
            name_length++;
        }
        while (name_length > 0u && descriptor[name_length - 1u] == ' ') {
            name_length--;
        }
        memcpy(field->name, descriptor, name_length);
        field->name[name_length] = '\0';
        field->type = (char)descriptor[11];
        field->width = (unsigned int)descriptor[16];
        field->decimals = (unsigned int)descriptor[17];
        field->offset = running_offset;

        validate_dbf_field(
            field->type,
            field->width,
            field->decimals,
            name_length,
            field->offset,
            reader->record_size
        );
        running_offset += field->width;

        if (!field->selected) {
            continue;
        }

        if (field->type == 'N' || field->type == 'F') {
            field->kind = DBF_COLUMN_NUMBER;
            field->integer_candidate = field->decimals == 0u;
            column = PROTECT(allocVector(REALSXP, reader->record_count));
        } else if (field->type == 'L') {
            field->kind = DBF_COLUMN_LOGICAL;
            column = PROTECT(allocVector(LGLSXP, reader->record_count));
        } else {
            field->kind = DBF_COLUMN_STRING;
            column = PROTECT(allocVector(STRSXP, reader->record_count));
        }

        SET_VECTOR_ELT(reader->data_frame, field->output_index, column);
        field->column = VECTOR_ELT(reader->data_frame, field->output_index);
        SET_STRING_ELT(
            names,
            field->output_index,
            mkCharLenCE(field->name, (int)name_length, CE_BYTES)
        );
        type_text[0] = field->type;
        type_text[1] = '\0';
        SET_STRING_ELT(
            reader->data_types,
            field->output_index,
            mkChar(type_text)
        );
        UNPROTECT(1);
    }

    if (running_offset != reader->record_size) {
        error("DBF field widths do not match the declared record size.");
    }

    setAttrib(reader->data_frame, R_NamesSymbol, names);
    setAttrib(reader->data_frame, install("data_types"), reader->data_types);
    header_snapshot = PROTECT(allocVector(RAWSXP, reader->header_size));
    memcpy(RAW(header_snapshot), reader->header, reader->header_size);
    setAttrib(reader->data_frame, install("dbf_header"), header_snapshot);
    UNPROTECT(3); /* names, data_types, and header snapshot are reachable */
}

static void finalize_integer_columns(dbc_reader *reader)
{
    unsigned int index;

    for (index = 0; index < reader->field_count; index++) {
        dbf_field *field = reader->fields + index;

        if (field->selected && field->kind == DBF_COLUMN_NUMBER &&
            field->integer_candidate) {
            R_xlen_t row;
            SEXP integer_column = PROTECT(
                allocVector(INTSXP, reader->record_count)
            );

            for (row = 0; row < (R_xlen_t)reader->record_count; row++) {
                double value = REAL(field->column)[row];
                INTEGER(integer_column)[row] = ISNA(value) ?
                    NA_INTEGER : (int)value;
            }
            SET_VECTOR_ELT(
                reader->data_frame,
                field->output_index,
                integer_column
            );
            field->column = VECTOR_ELT(
                reader->data_frame,
                field->output_index
            );
            UNPROTECT(1);
        }
    }
}

static void finish_data_frame(dbc_reader *reader)
{
    SEXP class_value = PROTECT(mkString("data.frame"));
    SEXP row_names;

    if (reader->record_count == 0u) {
        row_names = PROTECT(allocVector(INTSXP, 0));
    } else {
        row_names = PROTECT(allocVector(INTSXP, 2));
        INTEGER(row_names)[0] = NA_INTEGER;
        INTEGER(row_names)[1] = -(int)reader->record_count;
    }
    setAttrib(reader->data_frame, R_ClassSymbol, class_value);
    setAttrib(reader->data_frame, R_RowNamesSymbol, row_names);
    UNPROTECT(2);
}

static SEXP read_dbc_info_body(void *context)
{
    dbc_reader *reader = (dbc_reader *)context;
    unsigned int index;
    unsigned int running_offset = 1u;
    SEXP result;
    SEXP names;
    SEXP data_types;
    SEXP rows;
    SEXP language_driver;
    SEXP result_names;

    reader->file = fopen(reader->path, "rb");
    if (reader->file == NULL) {
        error("Unable to open DBC file.");
    }

    read_header(reader);
    validate_header_structure(reader);

    result = PROTECT(allocVector(VECSXP, 5));
    names = PROTECT(allocVector(STRSXP, reader->field_count));
    data_types = PROTECT(allocVector(STRSXP, reader->field_count));
    rows = PROTECT(ScalarInteger((int)reader->record_count));
    language_driver = PROTECT(ScalarInteger((int)reader->header[29]));
    result_names = PROTECT(allocVector(STRSXP, 5));

    for (index = 0; index < reader->field_count; index++) {
        const unsigned char *descriptor =
            reader->header + 32u + index * DBF_FIELD_DESCRIPTOR_SIZE;
        unsigned int name_length = 0u;
        unsigned int width = (unsigned int)descriptor[16];
        char type_text[2];

        while (name_length < 11u && descriptor[name_length] != '\0') {
            name_length++;
        }
        while (name_length > 0u && descriptor[name_length - 1u] == ' ') {
            name_length--;
        }
        validate_dbf_field(
            (char)descriptor[11],
            width,
            (unsigned int)descriptor[17],
            name_length,
            running_offset,
            reader->record_size
        );
        running_offset += width;

        SET_STRING_ELT(
            names,
            index,
            mkCharLenCE((const char *)descriptor, (int)name_length, CE_BYTES)
        );
        type_text[0] = (char)descriptor[11];
        type_text[1] = '\0';
        SET_STRING_ELT(data_types, index, mkChar(type_text));
    }

    if (running_offset != reader->record_size) {
        error("DBF field widths do not match the declared record size.");
    }

    SET_VECTOR_ELT(result, 0, names);
    SET_VECTOR_ELT(result, 1, data_types);
    SET_VECTOR_ELT(result, 2, rows);
    SET_VECTOR_ELT(result, 3, language_driver);
    {
        SEXP header_snapshot = PROTECT(
            allocVector(RAWSXP, reader->header_size)
        );
        memcpy(RAW(header_snapshot), reader->header, reader->header_size);
        SET_VECTOR_ELT(result, 4, header_snapshot);
        UNPROTECT(1);
    }
    SET_STRING_ELT(result_names, 0, mkChar("names"));
    SET_STRING_ELT(result_names, 1, mkChar("data_types"));
    SET_STRING_ELT(result_names, 2, mkChar("rows"));
    SET_STRING_ELT(result_names, 3, mkChar("language_driver"));
    SET_STRING_ELT(result_names, 4, mkChar("header"));
    setAttrib(result, R_NamesSymbol, result_names);

    UNPROTECT(6);
    return result;
}

static SEXP read_dbc_body(void *context)
{
    dbc_reader *reader = (dbc_reader *)context;
    dbc_input input;
    int blast_result;
    unsigned int compressed_left = 0u;
    unsigned char *compressed_next = NULL;
    unsigned char checksum_bytes[4];
    uint32_t actual_checksum;
    SEXP result;

    reader->file = fopen(reader->path, "rb");
    if (reader->file == NULL) {
        error("Unable to open DBC file.");
    }

    read_header(reader);
    parse_header(reader);

    if (reader->header_size > (size_t)LONG_MAX ||
        fseek(reader->file, (long)reader->header_size, SEEK_SET) != 0) {
        error("Failed to seek to the DBC checksum.");
    }
    if (fread(checksum_bytes, 1, DBC_CRC_SIZE, reader->file) != DBC_CRC_SIZE) {
        error("Failed to read the DBC checksum.");
    }
    reader->expected_checksum = read_le32(checksum_bytes);
    dbc_crc32_start(&reader->checksum);
    dbc_crc32_update(&reader->checksum, reader->header, reader->header_size);

    input.file = reader->file;
    input.reader = reader;
    blast_result = blast(
        dbc_input_callback,
        &input,
        dbc_output_callback,
        reader,
        &compressed_left,
        &compressed_next
    );

    if (reader->callback_failed) {
        error("Failed to parse decompressed DBF records: %s", reader->error);
    }
    if (blast_result != 0) {
        switch (blast_result) {
        case 2:
            error("Compressed DBC data ended unexpectedly.");
        case 1:
            error("Failed while consuming decompressed DBC data.");
        case -1:
            error("Invalid literal flag in compressed DBC data.");
        case -2:
            error("Invalid dictionary size in compressed DBC data.");
        case -3:
            error("Invalid backward distance in compressed DBC data.");
        default:
            error("DBC decompression failed.");
        }
    }
    if (compressed_left > 0u || fgetc(reader->file) != EOF) {
        error("Unexpected data after the compressed DBC stream.");
    }
    if (ferror(reader->file)) {
        error("Failed while checking the end of the DBC file.");
    }
    if (reader->row != reader->record_count || reader->record_used != 0u) {
        error(
            "DBC decompression produced %u of %u declared DBF records.",
            reader->row,
            reader->record_count
        );
    }
    actual_checksum = dbc_crc32_finish(&reader->checksum);
    if (actual_checksum != reader->expected_checksum) {
        error(
            "DBC checksum mismatch (expected %08x, got %08x).",
            (unsigned int)reader->expected_checksum,
            (unsigned int)actual_checksum
        );
    }

    finalize_integer_columns(reader);
    finish_data_frame(reader);
    if (reader->invalid_numerics > 0u) {
        warning(
            "%u invalid numeric value%s found in DBF fields; converted to "
            "NA (first at field %u, record %u)",
            reader->invalid_numerics,
            reader->invalid_numerics == 1u ? "" : "s",
            reader->first_invalid_numeric_field,
            reader->first_invalid_numeric_row
        );
    }
    if (reader->invalid_logicals > 0u) {
        warning(
            "%u invalid value%s found in DBF logical fields; converted to NA",
            reader->invalid_logicals,
            reader->invalid_logicals == 1u ? "" : "s"
        );
    }

    result = reader->data_frame;
    UNPROTECT(1); /* data_frame */
    return result;
}

SEXP microdatasus_dbc_info(SEXP file)
{
    dbc_reader reader;
    const char *path;

    if (TYPEOF(file) != STRSXP || XLENGTH(file) != 1 ||
        STRING_ELT(file, 0) == NA_STRING) {
        error("file must be one non-missing character string");
    }
    path = CHAR(STRING_ELT(file, 0));
    if (path[0] == '\0') {
        error("file must not be empty");
    }

    memset(&reader, 0, sizeof(reader));
    reader.path = path;

    return R_UnwindProtect(
        read_dbc_info_body,
        &reader,
        reader_cleanup,
        &reader,
        NULL
    );
}

SEXP microdatasus_read_dbc(SEXP file, SEXP selection)
{
    dbc_reader reader;
    const char *path;

    if (TYPEOF(file) != STRSXP || XLENGTH(file) != 1 ||
        STRING_ELT(file, 0) == NA_STRING) {
        error("file must be one non-missing character string");
    }
    path = CHAR(STRING_ELT(file, 0));
    if (path[0] == '\0') {
        error("file must not be empty");
    }

    memset(&reader, 0, sizeof(reader));
    reader.path = path;
    reader.selection = selection;

    return R_UnwindProtect(
        read_dbc_body,
        &reader,
        reader_cleanup,
        &reader,
        NULL
    );
}
