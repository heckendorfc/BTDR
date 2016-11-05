#include "populate.h"
#include "utils.h"

SEXP makedf_scan(struct iobtd *iop){
	SEXP df, plidvec, sidvec, mzvec, zvec, rtvec;
	yamldom_node_t *peakseq, *scanseq, *tmp, *seq, *scan;
	int i, count, peakcount;
	int id;
	const int ncols=5;

	if(!(scanseq=yamldom_find_map_val(iop->root,"scan"))){
		return RNULL;
	}

	if(!(peakseq=yamldom_find_map_val(iop->root,"peaks"))){
		return RNULL;
	}

	count=0;
	for(seq=YAMLDOM_SEQ_NODES(peakseq);seq;seq=seq->next){
		if(!(tmp=yamldom_find_map_val(seq,"scans")))
			return RNULL;
		count += count_seq_elem(tmp);
	}

	PROTECT(sidvec = allocVector(INTSXP,count));
	PROTECT(plidvec = allocVector(INTSXP,count));
	PROTECT(mzvec = allocVector(REALSXP,count));
	PROTECT(zvec = allocVector(INTSXP,count));
	PROTECT(rtvec = allocVector(REALSXP,count));

	i=0;
	for(seq=YAMLDOM_SEQ_NODES(peakseq);seq;seq=seq->next){
		if(!(tmp=yamldom_find_map_val(seq,"id")))
			return RNULL;
		id=strtol(((yamldom_scalar_t*)tmp->data)->val,NULL,10);

		if(!(scanseq=yamldom_find_map_val(seq,"scans")))
			return RNULL;

		for(tmp=YAMLDOM_SEQ_NODES(scanseq);tmp;tmp=tmp->next){
			scan=((yamldom_alias_t*)tmp->data)->ref;

			INTEGER(plidvec)[i]=id;
			push_elem(INTEGER(sidvec),i,scan,"id",strtoint);
			push_elem(INTEGER(zvec),i,scan,"z",strtoint);
			push_elem(REAL(mzvec),i,scan,"mz",strtodouble);
			push_elem(REAL(rtvec),i,scan,"rt",strtodouble);
			if(REAL(rtvec)[i]<0.)
				REAL(rtvec)[i]=0.;

			scanseq = scanseq->next;
			i++;
		}
	}

	PROTECT(df = make_dataframe(RNULL,
								make_list_names(ncols, "plid", "scanid", "mz", "z", "rt"),
								ncols, plidvec, sidvec, mzvec, zvec, rtvec));

	UNPROTECT(6);

	return df;
}
