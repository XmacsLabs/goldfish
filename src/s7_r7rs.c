/* s7_r7rs.c - R7RS specific implementations for s7 Scheme interpreter
 *
 * derived from s7, a Scheme interpreter
 * SPDX-License-Identifier: 0BSD
 *
 * Bill Schottstaedt, bil@ccrma.stanford.edu
 */

#ifndef HAVE_COMPLEX_NUMBERS
  #if __TINYC__ || (__clang__ && __cplusplus)
    #define HAVE_COMPLEX_NUMBERS 0
  #else
    #define HAVE_COMPLEX_NUMBERS 1
  #endif
#endif

#include "s7_r7rs.h"
#include <string.h>
#include <time.h>
#include <math.h>

#if WITH_R7RS

/* R7RS Scheme code string */
const char r7rs_scm[] = "";

/* -------------------------------- sqrt -------------------------------- */
/* Helper to check if a double is NaN */
static bool is_nan(double x)
{
  return x != x;
}

/* Helper to create complex number with 0 imaginary part optimized */
static s7_pointer make_complex_not_0i(s7_scheme *sc, double r, double i)
{
  if (i == 0.0) return s7_make_real(sc, r);
  return s7_make_complex(sc, r, i);
}

s7_pointer sqrt_p_p(s7_scheme *sc, s7_pointer num)
{
  if (s7_is_integer(num))
    {
      s7_int iv = s7_integer(num);
      if (iv >= 0)
        {
          double sqx = sqrt((double)iv);
          s7_int ix = (s7_int)sqx;
          if ((ix * ix) == iv)
            return s7_make_integer(sc, ix);
          return s7_make_real(sc, sqx);
        }
#if HAVE_COMPLEX_NUMBERS
      return make_complex_not_0i(sc, 0.0, sqrt((double)(-iv)));
#else
      return s7_out_of_range_error(sc, "sqrt", 1, num, "no complex numbers");
#endif
    }

  if (s7_is_rational(num) && !s7_is_integer(num))
    {
      s7_int numr = s7_numerator(num);
      if (numr > 0)
        {
          s7_int nm = (s7_int)sqrt((double)numr);
          if (nm * nm == numr)
            {
              s7_int den = s7_denominator(num);
              s7_int dn = (s7_int)sqrt((double)den);
              if (dn * dn == den)
                return s7_make_ratio(sc, nm, dn);
            }
          double frac = (double)numr / (double)s7_denominator(num);
          return s7_make_real(sc, sqrt(frac));
        }
#if HAVE_COMPLEX_NUMBERS
      double frac = (double)numr / (double)s7_denominator(num);
      return s7_make_complex(sc, 0.0, sqrt(-frac));
#else
      return s7_out_of_range_error(sc, "sqrt", 1, num, "no complex numbers");
#endif
    }

  if (s7_is_real(num))
    {
      double rv = s7_real(num);
      if (is_nan(rv)) return num;
      if (rv >= 0.0)
        return s7_make_real(sc, sqrt(rv));
      return make_complex_not_0i(sc, 0.0, sqrt(-rv));
    }

  if (s7_is_complex(num))
    {
#if HAVE_COMPLEX_NUMBERS
      double r = s7_real_part(num);
      double i = s7_imag_part(num);
      s7_complex z = r + i * _Complex_I;
      s7_complex result = csqrt(z);
      return s7_make_complex(sc, creal(result), cimag(result));
#else
      return s7_out_of_range_error(sc, "sqrt", 1, num, "no complex numbers");
#endif
    }

  return s7_wrong_type_arg_error(sc, "sqrt", 1, num, "a number");
}

s7_pointer g_sqrt(s7_scheme *sc, s7_pointer args)
{
  #define H_sqrt "(sqrt z) returns the square root of z"
  #define Q_sqrt sc->pl_nn
  return(sqrt_p_p(sc, s7_car(args)));
}

#endif /* WITH_R7RS */
