#include "populate.h"
#include "utils.h"

SEXP makedf_param(struct iobtd *iop){
	SEXP df, idvec, fragvec, mstolvec, msmstolvec, msmassvec, taxvec;
	yamldom_node_t *paramseq, *seq;
	int i, count;
	const int ncols=6;

	if(!(paramseq=yamldom_find_map_val(iop->root,"param"))){
		goto err;
	}

	count = count_seq_elem(paramseq);

	hidefromGC(idvec = allocVector(INTSXP,count));
	hidefromGC(fragvec = allocVector(STRSXP,count));
	hidefromGC(mstolvec = allocVector(REALSXP,count));
	hidefromGC(msmstolvec = allocVector(REALSXP,count));
	hidefromGC(msmassvec = allocVector(REALSXP,count));
	hidefromGC(taxvec = allocVector(STRSXP,count));

	i=0;
	for(seq=YAMLDOM_SEQ_NODES(paramseq);seq;seq=seq->next){
		push_elem(INTEGER(idvec),i,seq,"id",strtoint);
		push_elem(fragvec,i,seq,"frag",strtostr);
		push_elem(REAL(mstolvec),i,seq,"mstol",strtodouble);
		push_elem(REAL(msmstolvec),i,seq,"msmstol",strtodouble);
		push_elem(REAL(msmassvec),i,seq,"msmass",strtodouble);
		push_elem(taxvec,i,seq,"tax",strtostr);

		i++;
	}

	df = make_dataframe(RNULL,
						make_list_names(ncols, "id", "frag", "mstol", "msmstol", "msmass", "tax"),
						ncols, idvec, fragvec, mstolvec, msmstolvec, msmassvec, taxvec);

	unhideGC();

	return df;

err:
	unhideGC();
	return RNULL;
}
