#ifndef _INC_PHRQTYPE_H
#define _INC_PHRQTYPE_H
/*
 *   The following implements long double
 *   Many machines long double = double so there is no advantage
 *   Check float.h include file for number of digits (LDBL_DIG)
 *   Need to define here and in cl1.c
 */
#ifdef PHREEQC_IDENT
static char const svnidphrqtype[] =
	"$Id: phrqtype.h 3840 2009-12-03 00:47:19Z dlpark $";
#endif

/*#define USE_LONG_DOUBLE*/
#ifdef USE_LONG_DOUBLE
#define LDBLE long double
#define SCANFORMAT "%Lf"
#else
#define LDBLE double
#define SCANFORMAT "%lf"
#endif
#endif /* _INC_PHRQTYPE_H */
