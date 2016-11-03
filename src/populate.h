#ifndef __RPKG_POPULATE_H__
#define __RPKG_POPULATE_H__

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

#include "libyaml-dom/yamldom.h"
#include "RNACI/RNACI.h"

#define CHARPT(x,i)	((char*)CHAR(STRING_ELT(x,i)))
#define NULLDF(df) PROTECT(df = make_dataframe(RNULL, RNULL, 0)); \
		UNPROTECT(1);

struct iobtd{
	yamldom_data_t iod;
	yamldom_node_t *root;
	yamldom_anchor_list_t anchors;
};

SEXP makedf_scan(struct iobtd *iop);
SEXP makedf_peak(struct iobtd *iop);
SEXP makedf_param(struct iobtd *iop);
SEXP makedf_mod(struct iobtd *iop);
SEXP makedf_prot(struct iobtd *iop);
SEXP makedf_tag(struct iobtd *iop);
SEXP makedf_search(struct iobtd *iop);
SEXP makedf_fit(struct iobtd *iop);

#endif
