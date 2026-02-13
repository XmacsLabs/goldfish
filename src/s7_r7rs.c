/* s7_r7rs.c - R7RS specific implementations for s7 Scheme interpreter
 *
 * derived from s7, a Scheme interpreter
 * SPDX-License-Identifier: 0BSD
 *
 * Bill Schottstaedt, bil@ccrma.stanford.edu
 */

#include "s7_r7rs.h"
#include <string.h>
#include <time.h>

#if WITH_R7RS

/* R7RS specific symbols */
s7_pointer unlink_symbol, access_symbol, time_symbol, clock_gettime_symbol,
           getenvs_symbol, uname_symbol;

/* R7RS Scheme code string */
const char r7rs_scm[] = "";

#endif /* WITH_R7RS */
