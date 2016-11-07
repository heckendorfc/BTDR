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



#include "RNACI.h"

// ..//src/alloc.c
static inline SEXP __Rvecalloc(int n, char *type, int init)
{
  SEXP RET;
  
  if (strncmp(type, "vec", 1) == 0)
    PROTECT(RET = allocVector(VECSXP, n));
  else if (strncmp(type, "int", 1) == 0)
  {
    PROTECT(RET = allocVector(INTSXP, n));
    
    if (init)
      memset(INTP(RET), 0, n*sizeof(int));
  }
  else if (strncmp(type, "double", 1) == 0)
  {
    PROTECT(RET = allocVector(REALSXP, n));
    
    if (init)
      memset(DBLP(RET), 0, n*sizeof(double));
  }
  else if (strncmp(type, "str", 1) == 0 || strncmp(type, "char*", 1) == 0)
    PROTECT(RET = allocVector(STRSXP, n));
  else
    error("unknown allocation type\n");
  
  __RNACI_SEXP_protect_counter++;
  return RET;
}

static inline SEXP __Rmatalloc(int m, int n, char *type, int init)
{
  SEXP RET;
  
  if (strncmp(type, "vec", 1) == 0)
    PROTECT(RET = allocMatrix(VECSXP, m, n));
  else if (strncmp(type, "int", 1) == 0)
  {
    PROTECT(RET = allocMatrix(INTSXP, m, n));
    
    if (init)
      memset(INTP(RET), 0, m*n*sizeof(int));
  }
  else if (strncmp(type, "double", 1) == 0)
  {
    PROTECT(RET = allocMatrix(REALSXP, m, n));
    
    if (init)
      memset(DBLP(RET), 0, m*n*sizeof(double));
  }
  else if (strncmp(type, "str", 1) == 0 || strncmp(type, "char*", 1) == 0)
    PROTECT(RET = allocMatrix(STRSXP, m, n));
  else
    error("unknown allocation type\n");
  
  __RNACI_SEXP_protect_counter++;
  return RET;
}

static inline SEXP __Rsetclass(SEXP x, char *name)
{
  SEXP class;
  newRvec(class, 1, "str");
  SET_STRING_ELT(class, 0, mkChar(name));
  classgets(x, class);
  return class;
}



// ..//src/floats.c
 int fis_zero(double x)
{
  const double abs_eps = 1.1 * DBL_EPSILON;
  if (fabs(x) < abs_eps*DBL_MIN)
    return true;
  else
    return false;
}

 int fequals(double x, double y)
{
  const double abs_eps = 1.1 * DBL_EPSILON;
  const double diff = fabs(x - y);
  
  if (x == y)
    return true;
  else if (x == 0.0 || y == 0.0 || diff < DBL_MIN)
    return diff < (abs_eps*DBL_MIN);
  else
    return diff/(fabs(x) + fabs(y)) < abs_eps;
}



// ..//src/misc.c
 int is_Rnull(SEXP x)
{
  SEXP basePackage;
  SEXP tmp;
  
  RNACI_PT( basePackage = eval( lang2( install("getNamespace"), ScalarString(mkChar("base")) ), R_GlobalEnv ) );
  
  tmp = eval( lang2( install("is.null"), x), basePackage);
  
  UNPROTECT(1);
  return INT(tmp);
}

 int is_Rnan(SEXP x)
{
  SEXP basePackage;
  SEXP tmp;

  RNACI_PT( basePackage = eval( lang2( install("getNamespace"), ScalarString(mkChar("base")) ), R_GlobalEnv ) );

  tmp = eval( lang2( install("is.nan"), x), basePackage);

  UNPROTECT(1);
  return INT(tmp);
}

 int is_Rna(SEXP x)
{
  SEXP basePackage;
  SEXP tmp;
  
  RNACI_PT( basePackage = eval( lang2( install("getNamespace"), ScalarString(mkChar("base")) ), R_GlobalEnv ) );
  
  tmp = eval( lang2( install("is.na"), x), basePackage);
  
  UNPROTECT(1);
  return INT(tmp);
}

 int is_double(SEXP x)
{
  SEXP basePackage;
  SEXP tmp;
  
  RNACI_PT( basePackage = eval( lang2( install("getNamespace"), ScalarString(mkChar("base")) ), R_GlobalEnv ) );
  
  tmp = eval( lang2( install("is.double"), x), basePackage);
  
  UNPROTECT(1);
  return INT(tmp);
}

 int is_integer(SEXP x)
{
  SEXP basePackage;
  SEXP tmp;
  
  RNACI_PT( basePackage = eval( lang2( install("getNamespace"), ScalarString(mkChar("base")) ), R_GlobalEnv ) );
  
  tmp = eval( lang2( install("is.integer"), x), basePackage);
  
  UNPROTECT(1);
  return INT(tmp);
}



// ..//src/printing.c
 void PRINT(SEXP x)
{
  SEXP basePackage;
  
  PROTECT( basePackage = eval( lang2( install("getNamespace"), ScalarString(mkChar("base")) ), R_GlobalEnv ) );
  
  eval( lang2( install("print"), x), basePackage);
  
  UNPROTECT(1);
}



// ..//src/structures_dataframes.c
static inline SEXP make_dataframe_default_colnames(const int ncols)
{
  int buflen;
  SEXP ret;
  
  if (ncols == 0)
    return RNULL;
  
  buflen = (int) (ceil(log10((double)ncols)) + 1.);
  char *buf = (char*) R_alloc(buflen, sizeof(*buf));
  buf[0] = 'X';
  
  newRlist(ret, ncols);
  
  for (int i=0; i<ncols; i++)
  {
    sprintf(buf+1, "%d", i+1);
    buflen = (int) (ceil(log10((double)i+2)) + 1.);
    buflen = RNACI_MAX(buflen, 2);
    SET_VECTOR_ELT(ret, i, mkCharLen(buf, buflen));
  }
  
  return ret;
}

static inline SEXP make_dataframe_default_rownames(int nrows)
{
  int i;
  SEXP ret_names;
  
  newRvec(ret_names, nrows, "int");
  
  for (i=0; i<nrows; i++)
    INT(ret_names, i) = i + 1;
  
  return ret_names;
}

 SEXP make_dataframe(SEXP R_rownames, SEXP R_colnames, int ncols, ...)
{
  int nrows = 0;
  SEXP R_df;
  SEXP R_default_rownames;
  SEXP R_default_colnames;
  SEXP tmp;
  va_list listPointer;
  
  // Construct list
  newRlist(R_df, ncols);
  
  va_start(listPointer, ncols);
  
  for (int i=0; i<ncols; i++)
  {
    tmp = va_arg(listPointer, SEXP);
    
    SET_VECTOR_ELT(R_df, i, tmp);
  }
  
  va_end(listPointer);
  
  // Set names
  set_list_as_df(R_df);
  
  
  if (R_rownames == RNULL)
  {
    if (ncols)
      nrows = LENGTH(VECTOR_ELT(R_df, 0));
    
    R_default_rownames = make_dataframe_default_rownames(nrows);
    set_df_rownames(R_df, R_default_rownames);
  }
  else
    set_df_rownames(R_df, R_rownames);
  
  if (R_colnames == RNULL)
  {
    if (ncols == 0)
      R_default_colnames = make_dataframe_default_rownames(0);
    else
      R_default_colnames = RNULL;
    
    set_df_colnames(R_df, R_default_colnames);
  }
  else
    set_df_colnames(R_df, R_colnames);
  
  
  return R_df;
}



// ..//src/structures_lists.c
 SEXP make_list_names(int n, ...)
{
  int i;
  char *tmp;
  SEXP R_list_names;
  va_list listPointer;
  
  newRvec(R_list_names, n, "str");
  
  va_start(listPointer, n);
  
  for (i=0; i<n; i++)
  {
    tmp = va_arg(listPointer, char *);
  
    SET_STRING_ELT(R_list_names, i, mkChar(tmp));
  }
  
  va_end(listPointer);
  
  return R_list_names;
}

 SEXP make_list(SEXP R_list_names, const int n, ...)
{
  int i;
  SEXP tmp, R_list;
  va_list listPointer;
  
  newRlist(R_list, n);
  
  va_start(listPointer, n);
  
  for (i=0; i<n; i++)
  {
    tmp = va_arg(listPointer, SEXP);
  
    SET_VECTOR_ELT(R_list, i, tmp);
  }
  
  va_end(listPointer);
  
  if (R_list_names != RNULL)
    set_list_names(R_list, R_list_names);
  
  return R_list;
}



// ..//src/structures_misc.c
 void set_list_names(SEXP R_list, SEXP R_names)
{
  setAttrib(R_list, R_NamesSymbol, R_names);
}

 void set_df_rownames(SEXP R_df, SEXP R_rownames)
{
  setAttrib(R_df, R_RowNamesSymbol, R_rownames);
}

 void set_df_colnames(SEXP R_df, SEXP R_colnames)
{
  setAttrib(R_df, R_NamesSymbol, R_colnames);
}

 void set_list_as_df(SEXP R_list)
{
  setAttrib(R_list, R_ClassSymbol, mkString("data.frame"));
}



