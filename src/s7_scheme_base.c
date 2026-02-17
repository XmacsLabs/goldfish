/* s7_scheme_base.c - basic function implementations for s7 Scheme interpreter
 *
 * derived from s7, a Scheme interpreter
 * SPDX-License-Identifier: 0BSD
 *
 * Bill Schottstaedt, bil@ccrma.stanford.edu
 */

#include "s7_scheme_base.h"
#include <string.h>
#include <math.h>

/* Helper function to check for NaN */
bool is_NaN(s7_double x)
{
  return x != x;
}

/* Helper function to check for infinity */
static bool is_inf(s7_double x)
{
  return isinf(x);
}

#define DOUBLE_TO_INT64_LIMIT 9.223372036854775807e18  /* 2^63 - 1 */

/* -------------------------------- floor -------------------------------- */

s7_pointer floor_p_p(s7_scheme *sc, s7_pointer x)
{
  if (s7_is_integer(x))
    return x;

  if (s7_is_rational(x) && !s7_is_integer(x))
    {
      s7_int num = s7_numerator(x);
      s7_int den = s7_denominator(x);
      s7_int val = num / den;  /* C '/' truncates toward zero */
      /* For negative fractions, floor is val-1 */
      return s7_make_integer(sc, (num < 0) ? (val - 1) : val);
    }

  if (s7_is_real(x))
    {
      s7_double z = s7_real(x);
      if (is_NaN(z))
        return s7_out_of_range_error(sc, "floor", 1, x, "it is NaN");
      if (is_inf(z))
        return s7_out_of_range_error(sc, "floor", 1, x, "it is infinite");
      if (fabs(z) > DOUBLE_TO_INT64_LIMIT)
        return s7_out_of_range_error(sc, "floor", 1, x, "it is too large");
      return s7_make_integer(sc, (s7_int)floor(z));
    }

  if (s7_is_complex(x))
    return s7_wrong_type_arg_error(sc, "floor", 1, x, "a real number");

  return s7_wrong_type_arg_error(sc, "floor", 1, x, "a number");
}

s7_pointer g_floor(s7_scheme *sc, s7_pointer args)
{
  #define H_floor "(floor x) returns the integer closest to x toward -inf"
  #define Q_floor s7_make_signature(sc, 2, sc->is_integer_symbol, sc->is_real_symbol)
  return floor_p_p(sc, s7_car(args));
}

s7_int floor_i_i(s7_int i)
{
  return i;
}

s7_pointer floor_p_i(s7_scheme *sc, s7_int x)
{
  return s7_make_integer(sc, x);
}

s7_int floor_i_7d(s7_scheme *sc, s7_double x)
{
  if (is_NaN(x))
    s7_out_of_range_error(sc, "floor", 1, s7_make_real(sc, x), "it is NaN");
  if (fabs(x) > DOUBLE_TO_INT64_LIMIT)
    s7_out_of_range_error(sc, "floor", 1, s7_make_real(sc, x), "it is too large");
  return (s7_int)floor(x);
}

s7_int floor_i_7p(s7_scheme *sc, s7_pointer x)
{
  if (s7_is_integer(x))
    return s7_integer(x);
  if (s7_is_real(x))
    return floor_i_7d(sc, s7_real(x));
  if (s7_is_rational(x) && !s7_is_integer(x))
    {
      s7_int num = s7_numerator(x);
      s7_int den = s7_denominator(x);
      s7_int val = num / den;
      return (num < 0) ? val - 1 : val;
    }
  s7_wrong_type_arg_error(sc, "floor", 1, x, "a real number"); return 0;
}

s7_pointer floor_p_d(s7_scheme *sc, s7_double x)
{
  return s7_make_integer(sc, floor_i_7d(sc, x));
}

/* -------------------------------- ceiling -------------------------------- */

s7_pointer ceiling_p_p(s7_scheme *sc, s7_pointer x)
{
  if (s7_is_integer(x))
    return x;

  if (s7_is_rational(x) && !s7_is_integer(x))
    {
      s7_int num = s7_numerator(x);
      s7_int den = s7_denominator(x);
      s7_int val = num / den;  /* C '/' truncates toward zero */
      /* For positive fractions, ceiling is val+1; for negative fractions, ceiling is val */
      return s7_make_integer(sc, (num < 0) ? val : (val + 1));
    }

  if (s7_is_real(x))
    {
      s7_double z = s7_real(x);
      if (is_NaN(z))
        return s7_out_of_range_error(sc, "ceiling", 1, x, "it is NaN");
      if (is_inf(z))
        return s7_out_of_range_error(sc, "ceiling", 1, x, "it is infinite");
      if (fabs(z) > DOUBLE_TO_INT64_LIMIT)
        return s7_out_of_range_error(sc, "ceiling", 1, x, "it is too large");
      return s7_make_integer(sc, (s7_int)ceil(z));
    }

  if (s7_is_complex(x))
    return s7_wrong_type_arg_error(sc, "ceiling", 1, x, "a real number");

  return s7_wrong_type_arg_error(sc, "ceiling", 1, x, "a number");
}

s7_pointer g_ceiling(s7_scheme *sc, s7_pointer args)
{
  #define H_ceiling "(ceiling x) returns the integer closest to x toward inf"
  #define Q_ceiling s7_make_signature(sc, 2, sc->is_integer_symbol, sc->is_real_symbol)
  return ceiling_p_p(sc, s7_car(args));
}

s7_int ceiling_i_i(s7_int i)
{
  return i;
}

s7_pointer ceiling_p_i(s7_scheme *sc, s7_int x)
{
  return s7_make_integer(sc, x);
}

s7_int ceiling_i_7d(s7_scheme *sc, s7_double x)
{
  if (is_NaN(x))
    s7_out_of_range_error(sc, "ceiling", 1, s7_make_real(sc, x), "it is NaN");
  if (fabs(x) > DOUBLE_TO_INT64_LIMIT)
    s7_out_of_range_error(sc, "ceiling", 1, s7_make_real(sc, x), "it is too large");
  return (s7_int)ceil(x);
}

s7_int ceiling_i_7p(s7_scheme *sc, s7_pointer x)
{
  if (s7_is_integer(x))
    return s7_integer(x);
  if (s7_is_real(x))
    return ceiling_i_7d(sc, s7_real(x));
  if (s7_is_rational(x) && !s7_is_integer(x))
    {
      s7_int num = s7_numerator(x);
      s7_int den = s7_denominator(x);
      s7_int val = num / den;
      return (num < 0) ? val : (val + 1);
    }
  s7_wrong_type_arg_error(sc, "ceiling", 1, x, "a real number"); return 0;
}

s7_pointer ceiling_p_d(s7_scheme *sc, s7_double x)
{
  return s7_make_integer(sc, ceiling_i_7d(sc, x));
}