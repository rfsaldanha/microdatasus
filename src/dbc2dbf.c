/*
 * dbc2dbf.c -- decompress DATASUS .dbc files to .dbf format
 * Copyright (C) 2025 Sidney Bissoli (healthbR package)
 * License: MIT
 *
 * DBC files are DBF files compressed with the PKWare DCL implode algorithm.
 * The file layout is:
 *   offset 0x00..0x07: first 8 bytes of the original DBF header
 *   offset 0x08..0x09: uint16 LE = total DBF header size to copy (hdr_size)
 *   offset 0x0A..hdr_size-1: remaining uncompressed DBF header bytes
 *   offset hdr_size..hdr_size+3: CRC32 of the complete decompressed DBF
 *   offset hdr_size+4..: PKWare DCL compressed data (decompressed with blast)
 *
 * This code was written from scratch for the healthbR package.
 * It uses blast.c/blast.h by Mark Adler (zlib license) for decompression.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "blast.h"
#include "dbc_crc32.h"

struct input_state {
    FILE *file;
    unsigned char buffer[16384];
    int read_failed;
};

struct output_state {
    FILE *file;
    dbc_crc32_state checksum;
};

/* input callback for blast(): reads from a per-call buffer */
static unsigned inf(void *how, unsigned char **buf)
{
    struct input_state *input = (struct input_state *)how;
    size_t count;

    *buf = input->buffer;
    count = fread(input->buffer, 1, sizeof(input->buffer), input->file);
    if (count == 0u && ferror(input->file)) {
        input->read_failed = 1;
    }
    return (unsigned)count;
}

/* output callback for blast(): updates the DBF checksum and writes the bytes */
static int outf(void *how, unsigned char *buf, unsigned len)
{
    struct output_state *output = (struct output_state *)how;

    dbc_crc32_update(&output->checksum, buf, (size_t)len);
    return fwrite(buf, 1, len, output->file) != len;
}

/*
 * dbc2dbf - decompress a DATASUS .dbc file to .dbf
 *
 * Called from R via .C() with 4 arguments:
 *   input_file  - path to input .dbc file
 *   output_file - path to output .dbf file
 *   ret_code    - return code (0 = success, nonzero = error)
 *   error_str   - error message string
 */
void dbc2dbf(char **input_file, char **output_file,
             int *ret_code, char **error_str)
{
    FILE *fin = NULL;
    FILE *fout = NULL;
    unsigned char raw_hdr[2];
    unsigned char raw_crc[4];
    unsigned char *buf = NULL;
    uint16_t hdr_size;
    size_t nread, nwritten;
    int blast_ret;
    unsigned int compressed_left = 0u;
    unsigned char *compressed_next = NULL;
    uint32_t expected_checksum;
    struct input_state input;
    struct output_state output;

    *ret_code = 0;
    *error_str = "";

    /* open input .dbc file */
    fin = fopen(*input_file, "rb");
    if (fin == NULL) {
        *ret_code = 1;
        *error_str = "cannot open input .dbc file";
        return;
    }

    /* open output .dbf file */
    fout = fopen(*output_file, "wb");
    if (fout == NULL) {
        *ret_code = 2;
        *error_str = "cannot open output .dbf file";
        fclose(fin);
        return;
    }

    /* read the DBF header size from offset 8 (2-byte little-endian uint16) */
    if (fseek(fin, 8, SEEK_SET) != 0) {
        *ret_code = 3;
        *error_str = "failed to seek to header offset in .dbc file";
        fclose(fin);
        fclose(fout);
        remove(*output_file);
        return;
    }

    nread = fread(raw_hdr, 1, 2, fin);
    if (nread < 2) {
        *ret_code = 3;
        *error_str = "failed to read header size from .dbc file (file too small)";
        fclose(fin);
        fclose(fout);
        remove(*output_file);
        return;
    }

    hdr_size = (uint16_t)raw_hdr[0] | ((uint16_t)raw_hdr[1] << 8);

    if (hdr_size < 33u) {
        *ret_code = 4;
        *error_str = "invalid DBF header size in .dbc file";
        fclose(fin);
        fclose(fout);
        remove(*output_file);
        return;
    }

    /* rewind and copy hdr_size bytes from offset 0 to output (the DBF header) */
    if (fseek(fin, 0, SEEK_SET) != 0) {
        *ret_code = 5;
        *error_str = "failed to rewind .dbc file";
        fclose(fin);
        fclose(fout);
        remove(*output_file);
        return;
    }

    buf = (unsigned char *)malloc(hdr_size);
    if (buf == NULL) {
        *ret_code = 5;
        *error_str = "memory allocation failed for DBF header";
        fclose(fin);
        fclose(fout);
        remove(*output_file);
        return;
    }

    nread = fread(buf, 1, hdr_size, fin);
    if (nread != hdr_size) {
        *ret_code = 5;
        *error_str = "failed to read DBF header from .dbc file";
        free(buf);
        fclose(fin);
        fclose(fout);
        remove(*output_file);
        return;
    }

    nwritten = fwrite(buf, 1, hdr_size, fout);
    if (nwritten != hdr_size) {
        *ret_code = 6;
        *error_str = "failed to write DBF header to output file";
        free(buf);
        fclose(fin);
        fclose(fout);
        remove(*output_file);
        return;
    }

    dbc_crc32_start(&output.checksum);
    dbc_crc32_update(&output.checksum, buf, hdr_size);
    free(buf);
    output.file = fout;

    /* Read the little-endian CRC32. The compressed stream follows it. */
    if (fread(raw_crc, 1, sizeof(raw_crc), fin) != sizeof(raw_crc)) {
        *ret_code = 7;
        *error_str = "failed to read CRC32 from .dbc file";
        fclose(fin);
        fclose(fout);
        remove(*output_file);
        return;
    }
    expected_checksum = (uint32_t)raw_crc[0] |
        ((uint32_t)raw_crc[1] << 8) |
        ((uint32_t)raw_crc[2] << 16) |
        ((uint32_t)raw_crc[3] << 24);

    /* decompress the remaining data using blast */
    input.file = fin;
    input.read_failed = 0;
    blast_ret = blast(
        inf, &input, outf, &output, &compressed_left, &compressed_next
    );
    if (blast_ret != 0) {
        *ret_code = 8;
        switch (blast_ret) {
        case 2:
            *error_str = "compressed data ended unexpectedly";
            break;
        case 1:
            *error_str = "failed to write decompressed data";
            break;
        case -1:
            *error_str = "invalid literal flag in compressed data";
            break;
        case -2:
            *error_str = "invalid dictionary size in compressed data";
            break;
        case -3:
            *error_str = "invalid backward distance in compressed data";
            break;
        default:
            *error_str = "blast decompression failed";
            break;
        }
        fclose(fin);
        fclose(fout);
        remove(*output_file);
        return;
    }

    if (input.read_failed) {
        *ret_code = 8;
        *error_str = "failed to read compressed data";
        fclose(fin);
        fclose(fout);
        remove(*output_file);
        return;
    }
    if (compressed_left > 0u || fgetc(fin) != EOF) {
        *ret_code = 8;
        *error_str = "unexpected data after compressed stream";
        fclose(fin);
        fclose(fout);
        remove(*output_file);
        return;
    }
    if (ferror(fin)) {
        *ret_code = 8;
        *error_str = "failed while checking end of input file";
        fclose(fin);
        fclose(fout);
        remove(*output_file);
        return;
    }
    if (dbc_crc32_finish(&output.checksum) != expected_checksum) {
        *ret_code = 10;
        *error_str = "DBC CRC32 checksum mismatch";
        fclose(fin);
        fclose(fout);
        remove(*output_file);
        return;
    }

    fclose(fin);
    if (fclose(fout) != 0) {
        *ret_code = 9;
        *error_str = "failed to finalize output .dbf file";
        remove(*output_file);
        return;
    }

    *ret_code = 0;
    *error_str = "";
}
