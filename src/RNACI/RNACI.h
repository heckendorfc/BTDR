// NOTE: file generated automatically from RNACI source; do not edit by hand

// Copyright (c) 2014-2016, Drew Schmidt
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
// 1. Redistributions of source code must retain the above copyright notice, this
//   list of conditions and the following disclaimer.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
// ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

// Changelog:
// Version 0.4.0:
//   * Clean up internals; better internal guarding.
//   * Deprecate non-double float functions.
//   * Create build system for non-header-only uses.
//   * Fixed dataframe naming bug (Christian Heckendorf).
//   * Fixed segfault when creating 0-len dataframes in make_dataframe().
// 
// Version 0.3.0:
//   * Fixed warnings visible with -Wall -pedantic.
//   * Use strnlen() over strlen(); shorten string checks in allocator.
//   * Simplify initializer in allocator using memset().
// 
// Version 0.2.0:
//   * Converted to header only.
// 
// Version 0.1.0:
//   * Initial release.


#ifndef __RNACI_H__
#define __RNACI_H__


#ifndef __RNACI_API_H_
#define __RNACI_API_H_


#include <R.h>
#include <Rinternals.h>

#include <stdarg.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>
#include <float.h>


// Internals, please excuse the mess
#define RNACI_VERSION 0.4.0

#define RNACI_MAX(m,n) m<n?n:m

#define RNACI_IGNORED -1

extern unsigned int __RNACI_SEXP_protect_counter;

#define __RNACI_INT(x,y,...) INTEGER(x)[y]
#define __RNACI_DBL(x,y,...) REAL(x)[y]
#define __RNACI_STR(x,y,...) ((char*)CHAR(STRING_ELT(x,y)))

#define RNACI_PT(x) PROTECT((x)); (__RNACI_SEXP_protect_counter)++

#define OPTIONALARG1(a,b,c,...) (a),(b),(c)



// defs
#define RNULL R_NilValue

// Access SEXP by value
#define INT(...) __RNACI_INT(__VA_ARGS__,0,RNACI_IGNORED)
#define DBL(...) __RNACI_DBL(__VA_ARGS__,0,RNACI_IGNORED)
#define STR(...) __RNACI_STR(__VA_ARGS__,0,RNACI_IGNORED)

// SEXP data pointer
#define MatINT(x,i,j) (INTEGER(x)[i+nrows(x)*j])
#define MatDBL(x,i,j) (REAL(x)[i+nrows(x)*j])

#define INTP(x) (INTEGER(x))
#define DBLP(x) (REAL(x))

// gc guards
#define R_INIT 

#define R_END UNPROTECT(__RNACI_SEXP_protect_counter); __RNACI_SEXP_protect_counter=0;

#define hidefromGC(x) RNACI_PT(x)
#define unhideGC() R_END

// External pointers
#define newRptr(ptr,Rptr,fin) PROTECT(Rptr = R_MakeExternalPtr(ptr, R_NilValue, R_NilValue));R_RegisterCFinalizerEx(Rptr, fin, TRUE);__RNACI_SEXP_protect_counter++;
#define getRptr(ptr) R_ExternalPtrAddr(ptr);

#define newRptrfreefun(FNAME,TYPE,FREEFUN) \
void FNAME(SEXP ptr) \
{ \
  if (NULL == R_ExternalPtrAddr(ptr)) return; \
  TYPE *tmp = (TYPE *) R_ExternalPtrAddr(ptr); \
  FREEFUN(tmp); \
  R_ClearExternalPtr(ptr); \
} \
void __ignore_me_just_here_for_semicolons();

// allocators
#define newRlist(x,n) (x=__Rvecalloc(n, "vec", false))
// #define newRvec(x,n,type) RNACI_PT(x=__Rvecalloc(n, type,false))
#define newRvec(x,...) (x=__Rvecalloc(OPTIONALARG1(__VA_ARGS__,false,RNACI_IGNORED)))
// #define newRmat(x,m,n,type) RNACI_PT(x=__Rmatalloc(m,n,type,false))
#define newRmat(x,m,...) (x=__Rmatalloc(m,OPTIONALARG1(__VA_ARGS__,false,RNACI_IGNORED)))

#define setRclass(x,name) __Rsetclass(x, name);

// misc
#define Rputchar(c) Rprintf("%c", c)



// floats.c
int fis_zero(double x);
int fequals(double x, double y);

// misc.c
int is_Rnull(SEXP x);
int is_Rnan(SEXP x);
int is_Rna(SEXP x);
int is_double(SEXP x);
int is_integer(SEXP x);

// printinc.c
void PRINT(SEXP x);

// structures_dataframes.c
SEXP make_dataframe(SEXP R_rownames, SEXP R_colnames, int n, ...);

// structures_lists.c
SEXP make_list_names(int n, ...);
SEXP make_list(SEXP R_list_names, const int n, ...);

// structures_misc.c
void set_list_names(SEXP R_list, SEXP R_names);
void set_df_rownames(SEXP R_df, SEXP R_rownames);
void set_df_colnames(SEXP R_df, SEXP R_colnames);
void set_list_as_df(SEXP R_list);


#endif

#endif
