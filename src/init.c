/* init.c -- register C routines for healthbR package */

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* declaration from dbc2dbf.c */
extern void dbc2dbf(char **input_file, char **output_file,
                    int *ret_code, char **error_str);

static const R_CMethodDef CEntries[] = {
    {"healthbR_dbc2dbf", (DL_FUNC) &dbc2dbf, 4},
    {NULL, NULL, 0}
};

void R_init_healthbR(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
