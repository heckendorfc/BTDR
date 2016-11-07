#include "populate.h"
#include "utils.h"

SEXP makedf_param(struct iobtd *iop){
	SEXP df, idvec, fragvec, mstolvec, msmstolvec, msmassvec, taxvec;
	yamldom_node_t *paramseq, *tmp, *seq;
	int i, istart, count, peakcount;
	int id;
	const int ncols=6;

	if(!(paramseq=yamldom_find_map_val(iop->root,"param"))){
		return RNULL;
	}

	count = count_seq_elem(paramseq);

	hidefromGC(idvec = allocVector(INTSXP,count));
	hidefromGC(fragvec = allocVector(STRSXP,count));
	hidefromGC(mstolvec = allocVector(REALSXP,count));
	hidefromGC(msmstolvec = allocVector(REALSXP,count));
	hidefromGC(msmassvec = allocVector(REALSXP,count));
	hidefromGC(taxvec = allocVector(STRSXP,count));

	istart=i=0;
	for(seq=YAMLDOM_SEQ_NODES(paramseq);seq;seq=seq->next){
		if(!(tmp=yamldom_find_map_val(seq,"peaks")))
			return RNULL;
		push_elem(INTEGER(idvec),i,((yamldom_alias_t*)tmp->data)->ref,"id",strtoint);

		push_elem(fragvec,i,seq,"frag",strtostr);
		push_elem(REAL(mstolvec),i,seq,"mstol",strtodouble);
		push_elem(REAL(msmstolvec),i,seq,"msmstol",strtodouble);
		push_elem(REAL(msmassvec),i,seq,"msmass",strtodouble);
		push_elem(taxvec,i,seq,"tax",strtostr);

		i++;
	}

	hidefromGC(df = make_dataframe(RNULL,
								make_list_names(ncols, "peakid", "frag", "mstol", "msmstol", "msmass", "tax"),
								ncols, idvec, fragvec, mstolvec, msmstolvec, msmassvec, taxvec));

	return df;
}
