/* s7_r7rs.h - R7RS specific declarations for s7 Scheme interpreter
 *
 * derived from s7, a Scheme interpreter
 * SPDX-License-Identifier: 0BSD
 *
 * Bill Schottstaedt, bil@ccrma.stanford.edu
 */

#ifndef S7_R7RS_H
#define S7_R7RS_H

#include "s7.h"

#ifdef __cplusplus
extern "C" {
#endif

/* R7RS specific function declarations */
#if WITH_R7RS
/* R7RS initialization and support functions */
extern void r7rs_init(s7_scheme *sc);
extern void initialize_r7rs_support(s7_scheme *sc);
extern void setup_r7rs_features(s7_scheme *sc);

/* R7RS specific symbols */
extern s7_pointer unlink_symbol, access_symbol, time_symbol, clock_gettime_symbol,
                  getenvs_symbol, uname_symbol;

/* R7RS specific function declarations */
extern s7_pointer g_getenvs(s7_scheme *sc, s7_pointer args);

/* R7RS Scheme code string */
extern const char r7rs_scm[];

#endif /* WITH_R7RS */

#ifdef __cplusplus
}
#endif

#endif /* S7_R7RS_H */