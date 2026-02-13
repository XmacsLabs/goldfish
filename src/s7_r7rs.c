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

/* -------------------------------- getenvs -------------------------------- */
extern char **environ;

s7_pointer g_getenvs(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p = s7_nil(sc);
  for (int32_t i = 0; environ[i]; i++)
    {
      const char *eq;
      s7_pointer name, value;
      eq = strchr((const char *)environ[i], (int)'=');
      name = s7_make_string_with_length(sc, environ[i], eq - environ[i]);
      value = s7_make_string(sc, (char *)(eq + 1));
      p = s7_cons(sc, s7_cons(sc, name, value), p);
    }
  return(p);
}

#endif /* WITH_R7RS */
