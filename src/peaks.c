#include "populate.h"
#include "utils.h"

SEXP makedf_peak(struct iobtd *iop){
	SEXP df, idvec, massvec, intvec, zvec;
	yamldom_node_t *peakseq, *tmp, *seq;
	int i, istart, count, peakcount;
	int id;
	const int ncols=4;

	if(!(peakseq=yamldom_find_map_val(iop->root,"peaks"))){
		return RNULL;
	}

	count=0;
	for(seq=YAMLDOM_SEQ_NODES(peakseq);seq;seq=seq->next){
		if(!(tmp=yamldom_find_map_val(seq,"num")))
			return RNULL;
		count += strtol(((yamldom_scalar_t*)tmp->data)->val,NULL,10);
	}

	PROTECT(idvec = allocVector(INTSXP,count));
	PROTECT(massvec = allocVector(REALSXP,count));
	PROTECT(intvec = allocVector(REALSXP,count));
	PROTECT(zvec = allocVector(INTSXP,count));

	istart=i=0;
	for(seq=YAMLDOM_SEQ_NODES(peakseq);seq;seq=seq->next){
		if(!(tmp=yamldom_find_map_val(seq,"num")))
			return RNULL;
		count = strtol(((yamldom_scalar_t*)tmp->data)->val,NULL,10);

		if(!(tmp=yamldom_find_map_val(seq,"id")))
			return RNULL;
		id = strtol(((yamldom_scalar_t*)tmp->data)->val,NULL,10);
		for(i=0;i<count;i++)
			INTEGER(idvec)[istart+i] = id;

		push_seq(REAL(massvec)+istart,seq,"mass",count,strtodouble);
		push_seq(REAL(intvec)+istart,seq,"intensity",count,strtodouble);
		push_seq(INTEGER(zvec)+istart,seq,"z",count,strtoint);

		istart += count;
	}

	PROTECT(df = make_dataframe(RNULL,
								make_list_names(ncols, "id", "mass", "intensity", "z"),
								ncols, idvec, massvec, intvec, zvec));

	UNPROTECT(5);

	return df;
}
