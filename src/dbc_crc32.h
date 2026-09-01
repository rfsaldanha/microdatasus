#ifndef MICRODATASUS_DBC_CRC32_H
#define MICRODATASUS_DBC_CRC32_H

#include <stddef.h>
#include <stdint.h>

typedef struct {
    uint32_t table[256];
    uint32_t value;
} dbc_crc32_state;

static void dbc_crc32_start(dbc_crc32_state *state)
{
    unsigned int index;

    for (index = 0u; index < 256u; index++) {
        uint32_t value = (uint32_t)index;
        unsigned int bit;

        for (bit = 0u; bit < 8u; bit++) {
            value = (value & UINT32_C(1)) != 0u ?
                (value >> 1) ^ UINT32_C(0xedb88320) : value >> 1;
        }
        state->table[index] = value;
    }
    state->value = UINT32_C(0xffffffff);
}

static void dbc_crc32_update(
    dbc_crc32_state *state,
    const unsigned char *data,
    size_t length
)
{
    size_t index;
    uint32_t value = state->value;

    for (index = 0u; index < length; index++) {
        unsigned int table_index =
            (unsigned int)((value ^ (uint32_t)data[index]) & UINT32_C(0xff));
        value = state->table[table_index] ^ (value >> 8);
    }
    state->value = value;
}

static uint32_t dbc_crc32_finish(const dbc_crc32_state *state)
{
    return state->value ^ UINT32_C(0xffffffff);
}

#endif
