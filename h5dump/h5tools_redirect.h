/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *  Project                 : vtkCSCS                                        *
 *  Module                  : h5dump.h                                       *
 *  Revision of last commit : $Rev: 1460 $                                   *
 *  Author of last commit   : $Author: soumagne $                            *
 *  Date of last commit     : $Date:: 2009-12-02 18:38:09 +0100 #$           *
 *                                                                           *
 *  Copyright (C) CSCS - Swiss National Supercomputing Centre.               *
 *  You may use modify and and distribute this code freely providing         *
 *  1) This copyright notice appears on all copies of source code            *
 *  2) An acknowledgment appears with any substantial usage of the code      *
 *  3) If this code is contributed to any other open source project, it      *
 *  must not be reformatted such that the indentation, bracketing or         *
 *  overall style is modified significantly.                                 *
 *                                                                           *
 *  This software is distributed WITHOUT ANY WARRANTY; without even the      *
 *  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef H5TOOLS_REDIRECT_H__
#define H5TOOLS_REDIRECT_H__

extern "C" {
#include "H5Fprivate.h"
}
#include <cstdio>
#include <iostream>
#include <sstream>

/*
 * The global output stream replacing stdout
 */
extern std::ostringstream output_stream;
/*
 * The global print rank when used with DSM
 */
extern int print_rank;

static inline int h5tools_redirect_printf(const char *format, ...)
{
  char buf[256];
  int nchars;
  va_list args;

  va_start(args, format);
  nchars = vsprintf(buf, format, args);
  output_stream << buf;
  va_end(args);
  return nchars;
}
#ifdef printf
#undef printf
#endif
#define printf h5tools_redirect_printf

static inline int h5tools_redirect_HDfprintf(FILE *stream, const char *fmt, ...)
{
  int         n=0, nout = 0;
  int         fwidth, prec;
  int         zerofill;
  int         leftjust;
  int         plussign;
  int         ldspace;
  int         prefix;
  char        modifier[8];
  int         conv;
  char        *rest, format_templ[128];
  const char  *s;
  va_list     ap;

  assert (fmt);

  va_start (ap, fmt);
  while (*fmt) {
    fwidth = prec = 0;
    zerofill = 0;
    leftjust = 0;
    plussign = 0;
    prefix = 0;
    ldspace = 0;
    modifier[0] = '\0';

    if ('%'==fmt[0] && '%'==fmt[1]) {
      output_stream << '%';
      fmt += 2;
      nout++;
    } else if ('%'==fmt[0]) {
      s = fmt + 1;

      /* Flags */
      while (HDstrchr ("-+ #", *s)) {
        switch (*s) {
        case '-':
          leftjust = 1;
          break;
        case '+':
          plussign = 1;
          break;
        case ' ':
          ldspace = 1;
          break;
        case '#':
          prefix = 1;
          break;
        } /*lint !e744 Switch statement doesn't _need_ default */
        s++;
      }

      /* Field width */
      if (HDisdigit (*s)) {
        zerofill = ('0'==*s);
        fwidth = (int)HDstrtol (s, &rest, 10);
        s = rest;
      } else if ('*'==*s) {
        fwidth = va_arg (ap, int);
        if (fwidth<0) {
          leftjust = 1;
          fwidth = -fwidth;
        }
        s++;
      }

      /* Precision */
      if ('.'==*s) {
        s++;
        if (HDisdigit (*s)) {
          prec = (int)HDstrtol (s, &rest, 10);
          s = rest;
        } else if ('*'==*s) {
          prec = va_arg (ap, int);
          s++;
        }
        if (prec<1) prec = 1;
      }

      /* Extra type modifiers */
      if (HDstrchr ("ZHhlqLI", *s)) {
        switch (*s) {
        /*lint --e{506} Don't issue warnings about constant value booleans */
        /*lint --e{774} Don't issue warnings boolean within 'if' always evaluates false/true */
        case 'H':
          if (sizeof(hsize_t)<sizeof(long)) {
            modifier[0] = '\0';
          } else if (sizeof(hsize_t)==sizeof(long)) {
            HDstrcpy (modifier, "l");
          } else {
            HDstrcpy (modifier, H5_PRINTF_LL_WIDTH);
          }
          break;
        case 'Z':
          if (sizeof(size_t)<sizeof(long)) {
            modifier[0] = '\0';
          } else if (sizeof(size_t)==sizeof(long)) {
            HDstrcpy (modifier, "l");
          } else {
            HDstrcpy (modifier, H5_PRINTF_LL_WIDTH);
          }
          break;
        default:
          /* Handle 'I64' modifier for Microsoft's "__int64" type */
          if(*s=='I' && *(s+1)=='6' && *(s+2)=='4') {
            modifier[0] = *s;
            modifier[1] = *(s+1);
            modifier[2] = *(s+2);
            modifier[3] = '\0';
            s+=2; /* Increment over 'I6', the '4' is taken care of below */
          } /* end if */
          else {
            /* Handle 'll' for long long types */
            if(*s=='l' && *(s+1)=='l') {
              modifier[0] = *s;
              modifier[1] = *s;
              modifier[2] = '\0';
              s++; /* Increment over first 'l', second is taken care of below */
            } /* end if */
            else {
              modifier[0] = *s;
              modifier[1] = '\0';
            } /* end else */
          } /* end else */
          break;
        }
        s++;
      }

      /* Conversion */
      conv = *s++;

      /* Create the format template */
      sprintf (format_templ, "%%%s%s%s%s%s",
          leftjust?"-":"", plussign?"+":"",
              ldspace?" ":"", prefix?"#":"", zerofill?"0":"");
      if (fwidth>0)
        sprintf (format_templ+HDstrlen(format_templ), "%d", fwidth);
      if (prec>0)
        sprintf (format_templ+HDstrlen(format_templ), ".%d", prec);
      if (*modifier)
        sprintf (format_templ+HDstrlen(format_templ), "%s", modifier);
      sprintf (format_templ+HDstrlen(format_templ), "%c", conv);


      /* Conversion */
      switch (conv) {
      case 'd':
      case 'i':
        if (!HDstrcmp(modifier, "h")) {
          short x = (short)va_arg (ap, int);
          n = printf (format_templ, x);
        } else if (!*modifier) {
          int x = va_arg (ap, int);
          n = printf (format_templ, x);
        } else if (!HDstrcmp (modifier, "l")) {
          long x = va_arg (ap, long);
          n = printf (format_templ, x);
        } else {
          int64_t x = va_arg(ap, int64_t);
          n = printf (format_templ, x);
        }
        break;

      case 'o':
      case 'u':
      case 'x':
      case 'X':
        if (!HDstrcmp (modifier, "h")) {
          unsigned short x = (unsigned short)va_arg (ap, unsigned int);
          n = printf (format_templ, x);
        } else if (!*modifier) {
          unsigned int x = va_arg (ap, unsigned int); /*lint !e732 Loss of sign not really occuring */
          n = printf (format_templ, x);
        } else if (!HDstrcmp (modifier, "l")) {
          unsigned long x = va_arg (ap, unsigned long); /*lint !e732 Loss of sign not really occuring */
          n = printf (format_templ, x);
        } else {
          uint64_t x = va_arg(ap, uint64_t); /*lint !e732 Loss of sign not really occuring */
          n = printf (format_templ, x);
        }
        break;

      case 'f':
      case 'e':
      case 'E':
      case 'g':
      case 'G':
        if (!HDstrcmp (modifier, "h")) {
          float x = (float) va_arg (ap, double);
          n = printf (format_templ, x);
        } else if (!*modifier || !HDstrcmp (modifier, "l")) {
          double x = va_arg (ap, double);
          n = printf (format_templ, x);
        } else {
          /*
           *                      * Some compilers complain when `long double' and
           *                                           * `double' are the same thing.
           *                                                                */
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
          long double x = va_arg (ap, long double);
          n = printf (format_templ, x);
#else
          double x = va_arg (ap, double);
          n = printf (format_templ, x);
#endif
        }
        break;

      case 'a':
        {
          haddr_t x = va_arg (ap, haddr_t); /*lint !e732 Loss of sign not really occuring */
          if (H5F_addr_defined(x)) {
            sprintf(format_templ, "%%%s%s%s%s%s",
                leftjust?"-":"", plussign?"+":"",
                    ldspace?" ":"", prefix?"#":"",
                        zerofill?"0":"");
            if (fwidth>0)
              sprintf(format_templ+HDstrlen(format_templ), "%d", fwidth);

            /*lint --e{506} Don't issue warnings about constant value booleans */
            /*lint --e{774} Don't issue warnings boolean within 'if' always evaluates false/true */
            if (sizeof(x)==H5_SIZEOF_INT) {
              HDstrcat(format_templ, "u");
            } else if (sizeof(x)==H5_SIZEOF_LONG) {
              HDstrcat(format_templ, "lu");
            } else if (sizeof(x)==H5_SIZEOF_LONG_LONG) {
              HDstrcat(format_templ, H5_PRINTF_LL_WIDTH);
              HDstrcat(format_templ, "u");
            }
            n = printf(format_templ, x);
          } else {
            HDstrcpy(format_templ, "%");
            if (leftjust)
              HDstrcat(format_templ, "-");
            if (fwidth)
              sprintf(format_templ+HDstrlen(format_templ), "%d", fwidth);
            HDstrcat(format_templ, "s");
            printf(format_templ, "UNDEF");
          }
        }
        break;

      case 'c':
        {
          char x = (char)va_arg (ap, int);
          n = printf (format_templ, x);
        }
        break;

      case 's':
      case 'p':
        {
          char *x = va_arg (ap, char*); /*lint !e64 Type mismatch not really occuring */
          n = printf (format_templ, x);
        }
        break;

      case 'n':
        format_templ[HDstrlen(format_templ)-1] = 'u';
        n = printf (format_templ, nout);
        break;

      case 't':
        {
          htri_t tri_var = va_arg (ap, htri_t);
          if (tri_var > 0) printf ("TRUE");
          else if (!tri_var) printf ("FALSE");
          else printf ("FAIL(%d)", (int)tri_var);
        }
        break;

      default:
        output_stream << format_templ;
        n = (int)HDstrlen (format_templ);
        break;
      }
      nout += n;
      fmt = s;
    } else {
      output_stream << *fmt;
      fmt++;
      nout++;
    }
  }
  va_end (ap);
  return nout;
} /* end HDfprintf() */

#ifdef HDfprintf
#undef HDfprintf
#endif
#define HDfprintf h5tools_redirect_HDfprintf

#endif /* H5TOOLS_REDIRECT_H__ */
