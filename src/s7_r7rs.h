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

/* R7RS specific symbols */
extern s7_pointer unlink_symbol, access_symbol, time_symbol, clock_gettime_symbol,
                  getenvs_symbol, uname_symbol;

/* R7RS specific function declarations */
s7_pointer g_getenvs(s7_scheme *sc, s7_pointer args);

/* R7RS Scheme code string */
extern const char r7rs_scm[];

#ifdef __cplusplus
}
#endif

#endif /* S7_R7RS_H */