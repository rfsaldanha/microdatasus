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
 *   offset hdr_size..hdr_size+3: 4-byte CRC32 (skipped)
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

/* input callback for blast(): reads from a FILE* */
static unsigned inf(void *how, unsigned char **buf)
{
    static unsigned char hold[16384];
    *buf = hold;
    return (unsigned)fread(hold, 1, sizeof(hold), (FILE *)how);
}

/* output callback for blast(): writes to a FILE* */
static int outf(void *how, unsigned char *buf, unsigned len)
{
    return fwrite(buf, 1, len, (FILE *)how) != len;
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
    unsigned char *buf = NULL;
    uint16_t hdr_size;
    size_t nread, nwritten;
    int blast_ret;

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
        return;
    }

    nread = fread(raw_hdr, 1, 2, fin);
    if (nread < 2) {
        *ret_code = 3;
        *error_str = "failed to read header size from .dbc file (file too small)";
        fclose(fin);
        fclose(fout);
        return;
    }

    hdr_size = (uint16_t)raw_hdr[0] | ((uint16_t)raw_hdr[1] << 8);

    if (hdr_size == 0 || hdr_size > 65535u) {
        *ret_code = 4;
        *error_str = "invalid DBF header size in .dbc file";
        fclose(fin);
        fclose(fout);
        return;
    }

    /* rewind and copy hdr_size bytes from offset 0 to output (the DBF header) */
    if (fseek(fin, 0, SEEK_SET) != 0) {
        *ret_code = 5;
        *error_str = "failed to rewind .dbc file";
        fclose(fin);
        fclose(fout);
        return;
    }

    buf = (unsigned char *)malloc(hdr_size);
    if (buf == NULL) {
        *ret_code = 5;
        *error_str = "memory allocation failed for DBF header";
        fclose(fin);
        fclose(fout);
        return;
    }

    nread = fread(buf, 1, hdr_size, fin);
    if (nread != hdr_size) {
        *ret_code = 5;
        *error_str = "failed to read DBF header from .dbc file";
        free(buf);
        fclose(fin);
        fclose(fout);
        return;
    }

    nwritten = fwrite(buf, 1, hdr_size, fout);
    free(buf);
    if (nwritten != hdr_size) {
        *ret_code = 6;
        *error_str = "failed to write DBF header to output file";
        fclose(fin);
        fclose(fout);
        return;
    }

    /* seek to hdr_size + 4 (skip 4-byte CRC32 after the DBF header) */
    if (fseek(fin, (long)hdr_size + 4, SEEK_SET) != 0) {
        *ret_code = 7;
        *error_str = "failed to seek past CRC in .dbc file";
        fclose(fin);
        fclose(fout);
        return;
    }

    /* decompress the remaining data using blast */
    blast_ret = blast(inf, fin, outf, fout, NULL, NULL);
    if (blast_ret != 0) {
        *ret_code = 8;
        *error_str = "blast decompression failed";
        fclose(fin);
        fclose(fout);
        return;
    }

    fclose(fin);
    fclose(fout);

    *ret_code = 0;
    *error_str = "";
}
