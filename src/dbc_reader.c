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

#define DBC_INPUT_BUFFER_SIZE 16384u
#define DBC_CRC_SIZE 4L
#define DBF_MIN_HEADER_SIZE 33u
#define DBF_FIELD_DESCRIPTOR_SIZE 32u
#define DBF_MAX_FIELD_WIDTH 255u
#define DBC_ERROR_SIZE 256u

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
    SEXP column;
} dbf_field;

typedef struct {
    FILE *file;
    unsigned char buffer[DBC_INPUT_BUFFER_SIZE];
} dbc_input;

typedef struct {
    const char *path;
    FILE *file;
    unsigned char *header;
    unsigned char *record;
    dbf_field *fields;
    size_t header_size;
    unsigned int record_size;
    unsigned int record_used;
    unsigned int field_count;
    uint32_t record_count;
    uint32_t row;
    unsigned int invalid_logicals;
    int callback_failed;
    char error[DBC_ERROR_SIZE];
    SEXP data_frame;
    SEXP data_types;
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

static int parse_dbf_record(dbc_reader *reader)
{
    unsigned int index;
    R_xlen_t row = (R_xlen_t)reader->row;

    for (index = 0; index < reader->field_count; index++) {
        dbf_field *field = reader->fields + index;
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
                    mkCharLen((const char *)value, (int)length)
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
            } else {
                reader->invalid_logicals++;
            }
            LOGICAL(field->column)[row] = logical_value;
        } else {
            double number = NA_REAL;

            if (length > 0u && value[0] != '*') {
                char buffer[DBF_MAX_FIELD_WIDTH + 1u];
                char *end = NULL;

                if (length > DBF_MAX_FIELD_WIDTH) {
                    set_reader_error(reader, "DBF numeric field is too wide");
                    return 1;
                }
                memcpy(buffer, value, length);
                buffer[length] = '\0';
                number = R_strtod(buffer, &end);
                if (end == buffer) {
                    number = NA_REAL;
                }
            }

            REAL(field->column)[row] = number;
            if (field->integer_candidate && !ISNA(number)) {
                if (number > (double)INT_MAX ||
                    number < -2147483646.0 ||
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

    *buffer = input->buffer;
    return (unsigned)fread(
        input->buffer,
        1,
        sizeof(input->buffer),
        input->file
    );
}

static int dbc_output_callback(void *context, unsigned char *buffer, unsigned len)
{
    dbc_reader *reader = (dbc_reader *)context;
    unsigned int position = 0;

    while (position < len) {
        unsigned int available;
        unsigned int remaining;
        unsigned int take;

        /* A conventional DBF may end in 0x1a. Once all declared records have
         * been read, trailing decompressed bytes are irrelevant to the table. */
        if (reader->row >= reader->record_count) {
            return 0;
        }

        available = reader->record_size - reader->record_used;
        remaining = len - position;
        take = remaining < available ? remaining : available;
        memcpy(reader->record + reader->record_used, buffer + position, take);
        reader->record_used += take;
        position += take;

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

/* Leaves data_frame protected for the duration of read_dbc_body(). */
static void parse_header(dbc_reader *reader)
{
    unsigned int index;
    unsigned int running_offset = 1u;
    unsigned int descriptor_bytes;
    SEXP names;

    if (!valid_dbf_version(reader->header[0])) {
        error("Unsupported or invalid DBF version in DBC file.");
    }
    if (reader->header[reader->header_size - 1u] != 0x0d &&
        reader->header[reader->header_size - 1u] != 0x00) {
        error("Invalid DBF field descriptor terminator in DBC file.");
    }

    reader->record_count = read_le32(reader->header + 4);
    reader->record_size = (unsigned int)read_le16(reader->header + 10);
    if (reader->record_size == 0u) {
        error("Invalid zero-length DBF record in DBC file.");
    }
    if (reader->record_count > (uint32_t)INT_MAX) {
        error("DBC file has more rows than an R data frame can represent.");
    }

    descriptor_bytes = (unsigned int)reader->header_size - DBF_MIN_HEADER_SIZE;
    if (descriptor_bytes % DBF_FIELD_DESCRIPTOR_SIZE != 0u) {
        error("Invalid DBF field descriptor section in DBC file.");
    }
    reader->field_count = descriptor_bytes / DBF_FIELD_DESCRIPTOR_SIZE;
    if (reader->field_count == 0u) {
        error("No DBF fields found in DBC file.");
    }

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

    reader->data_frame = PROTECT(allocVector(VECSXP, reader->field_count));
    names = PROTECT(allocVector(STRSXP, reader->field_count));
    reader->data_types = PROTECT(allocVector(STRSXP, reader->field_count));

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

        if (field->width == 0u ||
            field->offset > reader->record_size ||
            field->width > reader->record_size - field->offset) {
            error("DBF field layout exceeds the declared record size.");
        }
        running_offset += field->width;

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

        SET_VECTOR_ELT(reader->data_frame, index, column);
        field->column = VECTOR_ELT(reader->data_frame, index);
        SET_STRING_ELT(names, index, mkCharLen(field->name, (int)name_length));
        type_text[0] = field->type;
        type_text[1] = '\0';
        SET_STRING_ELT(reader->data_types, index, mkChar(type_text));
        UNPROTECT(1);
    }

    setAttrib(reader->data_frame, R_NamesSymbol, names);
    setAttrib(reader->data_frame, install("data_types"), reader->data_types);
    UNPROTECT(2); /* names and data_types; both are now reachable from df */
}

static void finalize_integer_columns(dbc_reader *reader)
{
    unsigned int index;

    for (index = 0; index < reader->field_count; index++) {
        dbf_field *field = reader->fields + index;

        if (field->kind == DBF_COLUMN_NUMBER && field->integer_candidate) {
            R_xlen_t row;
            SEXP integer_column = PROTECT(
                allocVector(INTSXP, reader->record_count)
            );

            for (row = 0; row < (R_xlen_t)reader->record_count; row++) {
                double value = REAL(field->column)[row];
                INTEGER(integer_column)[row] = ISNA(value) ?
                    NA_INTEGER : (int)value;
            }
            SET_VECTOR_ELT(reader->data_frame, index, integer_column);
            field->column = VECTOR_ELT(reader->data_frame, index);
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

static SEXP read_dbc_body(void *context)
{
    dbc_reader *reader = (dbc_reader *)context;
    dbc_input input;
    int blast_result;
    SEXP result;

    reader->file = fopen(reader->path, "rb");
    if (reader->file == NULL) {
        error("Unable to open DBC file.");
    }

    read_header(reader);
    parse_header(reader);

    if (reader->header_size > (size_t)LONG_MAX - DBC_CRC_SIZE ||
        fseek(reader->file, (long)reader->header_size + DBC_CRC_SIZE, SEEK_SET) != 0) {
        error("Failed to seek to the compressed DBC record area.");
    }

    input.file = reader->file;
    blast_result = blast(
        dbc_input_callback,
        &input,
        dbc_output_callback,
        reader,
        NULL,
        NULL
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
    if (reader->row != reader->record_count || reader->record_used != 0u) {
        error(
            "DBC decompression produced %u of %u declared DBF records.",
            reader->row,
            reader->record_count
        );
    }

    finalize_integer_columns(reader);
    finish_data_frame(reader);
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

SEXP microdatasus_read_dbc(SEXP file)
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
        read_dbc_body,
        &reader,
        reader_cleanup,
        &reader,
        NULL
    );
}
