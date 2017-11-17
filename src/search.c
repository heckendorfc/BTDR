#include "populate.h"
#include "utils.h"

SEXP makedf_search(struct iobtd *iop){
	SEXP df, df_names, sidvec, pidvec, rankvec, scorevec, tscorevec, covvec, fdrvec;
	yamldom_node_t *searchseq, *seq;
	int i, count, peakid;
	char id[10+10+1];
	const int ncols=7;

	if(!(searchseq=yamldom_find_map_val(iop->root,"search"))){
		goto err;
	}

	count = count_seq_elem(searchseq);

	if(count==0)
		goto err;

	hidefromGC(sidvec = allocVector(STRSXP,count));
	hidefromGC(pidvec = allocVector(INTSXP,count));
	hidefromGC(rankvec = allocVector(INTSXP,count));
	hidefromGC(scorevec = allocVector(REALSXP,count));
	hidefromGC(tscorevec = allocVector(REALSXP,count));
	hidefromGC(covvec = allocVector(REALSXP,count));
	hidefromGC(fdrvec = allocVector(REALSXP,count));

	i=0;
	for(seq=YAMLDOM_SEQ_NODES(searchseq);seq;seq=seq->next){
		set_searchid(seq,id,&peakid,NULL);

		SET_STRING_ELT(sidvec, i, mkChar(id));
		INTEGER(pidvec)[i] = peakid;

		push_elem(INTEGER(rankvec),i,seq,"id",strtoint);
		push_elem(REAL(scorevec),i,seq,"score",strtodouble);
		push_elem(REAL(tscorevec),i,seq,"tagscore",strtodouble);
		push_elem(REAL(covvec),i,seq,"cov",strtodouble);
		push_elem(REAL(fdrvec),i,seq,"fdr",strtodouble);
		i++;
	}

	make_list_names(df_names, ncols, "searchid", "peakid", "rank", "score", "tagscore", "cov", "fdr");
	make_dataframe(df, RNULL, df_names, ncols, sidvec, pidvec, rankvec, scorevec, tscorevec, covvec, fdrvec);

	unhideGC();

	return df;

err:
	unhideGC();
	return RNULL;
}
