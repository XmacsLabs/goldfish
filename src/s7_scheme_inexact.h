/* s7_scheme_inexact.h - inexact number declarations for s7 Scheme interpreter
 *
 * derived from s7, a Scheme interpreter
 * SPDX-License-Identifier: 0BSD
 *
 * Bill Schottstaedt, bil@ccrma.stanford.edu
 */

#ifndef S7_SCHEME_INEXACT_H
#define S7_SCHEME_INEXACT_H

#include "s7.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Helper function to check for NaN */
bool is_NaN(s7_double x);

/* inexact number function declarations */
s7_pointer sqrt_p_p(s7_scheme *sc, s7_pointer num);
s7_pointer g_sqrt(s7_scheme *sc, s7_pointer args);

/* nan? function */
bool s7_is_nan(s7_scheme *sc, s7_pointer x);
s7_pointer g_is_nan(s7_scheme *sc, s7_pointer args);

#ifdef __cplusplus
}
#endif

#endif /* S7_SCHEME_INEXACT_H */
