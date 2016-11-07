#include "populate.h"
#include "utils.h"

SEXP makedf_search(struct iobtd *iop){
	SEXP df, sidvec, pidvec, rankvec, scorevec, tscorevec, covvec;
	yamldom_node_t *searchseq, *tmp, *seq;
	int i, count, peakid;
	char id[10+10+1];
	const int ncols=6;

	if(!(searchseq=yamldom_find_map_val(iop->root,"search"))){
		return RNULL;
	}

	count = count_seq_elem(searchseq);

	if(count==0)
		return RNULL;

	hidefromGC(sidvec = allocVector(STRSXP,count));
	hidefromGC(pidvec = allocVector(INTSXP,count));
	hidefromGC(rankvec = allocVector(INTSXP,count));
	hidefromGC(scorevec = allocVector(REALSXP,count));
	hidefromGC(tscorevec = allocVector(REALSXP,count));
	hidefromGC(covvec = allocVector(REALSXP,count));

	i=0;
	for(seq=YAMLDOM_SEQ_NODES(searchseq);seq;seq=seq->next){
		set_searchid(seq,id,&peakid,NULL);

		SET_STRING_ELT(sidvec, i, mkChar(id));
		INTEGER(pidvec)[i] = peakid;

		push_elem(INTEGER(rankvec),i,seq,"id",strtoint);
		push_elem(REAL(scorevec),i,seq,"score",strtodouble);
		push_elem(REAL(tscorevec),i,seq,"tagscore",strtodouble);
		push_elem(REAL(covvec),i,seq,"cov",strtodouble);
		i++;
	}

	hidefromGC(df = make_dataframe(RNULL,
								make_list_names(ncols, "searchid", "peakid", "rank", "score", "tagscore", "cov"),
								ncols, sidvec, pidvec, rankvec, scorevec, tscorevec, covvec));

	return df;
}
