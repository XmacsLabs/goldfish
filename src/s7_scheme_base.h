/* s7_scheme_base.h - basic function declarations for s7 Scheme interpreter
 *
 * derived from s7, a Scheme interpreter
 * SPDX-License-Identifier: 0BSD
 *
 * Bill Schottstaedt, bil@ccrma.stanford.edu
 */

#ifndef S7_SCHEME_BASE_H
#define S7_SCHEME_BASE_H

#include "s7.h"

#ifdef __cplusplus
extern "C" {
#endif

/* floor function */
s7_pointer floor_p_p(s7_scheme *sc, s7_pointer x);
s7_pointer g_floor(s7_scheme *sc, s7_pointer args);
s7_pointer floor_p_d(s7_scheme *sc, s7_double x);
s7_int floor_i_7d(s7_scheme *sc, s7_double x);
s7_int floor_i_7p(s7_scheme *sc, s7_pointer x);
s7_int floor_i_i(s7_int i);
s7_pointer floor_p_i(s7_scheme *sc, s7_int x);

#ifdef __cplusplus
}
#endif

#endif /* S7_SCHEME_BASE_H */