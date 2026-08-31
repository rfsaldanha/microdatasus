/* init.c -- register C routines for microdatasus package */

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* declaration from dbc2dbf.c */
extern void dbc2dbf(char **input_file, char **output_file,
                    int *ret_code, char **error_str);
extern SEXP microdatasus_read_dbc(SEXP file);

static const R_CMethodDef CEntries[] = {
    {"microdatasus_dbc2dbf", (DL_FUNC) &dbc2dbf, 4},
    {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
    {"microdatasus_read_dbc", (DL_FUNC) &microdatasus_read_dbc, 1},
    {NULL, NULL, 0}
};

void R_init_microdatasus(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
