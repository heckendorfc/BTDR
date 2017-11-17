#include "populate.h"
#include "utils.h"

SEXP makedf_peak(struct iobtd *iop){
	SEXP df, df_names, idvec, massvec, intvec, zvec;
	yamldom_node_t *peakseq, *tmp, *seq;
	int i, istart, count;
	int id;
	const int ncols=4;

	if(!(peakseq=yamldom_find_map_val(iop->root,"peaks"))){
		goto err;
	}

	count=0;
	for(seq=YAMLDOM_SEQ_NODES(peakseq);seq;seq=seq->next){
		if(!(tmp=yamldom_find_map_val(seq,"num")))
			goto err;
		count += strtol(((yamldom_scalar_t*)tmp->data)->val,NULL,10);
	}

	hidefromGC(idvec = allocVector(INTSXP,count));
	hidefromGC(massvec = allocVector(REALSXP,count));
	hidefromGC(intvec = allocVector(REALSXP,count));
	hidefromGC(zvec = allocVector(INTSXP,count));

	istart=i=0;
	for(seq=YAMLDOM_SEQ_NODES(peakseq);seq;seq=seq->next){
		if(!(tmp=yamldom_find_map_val(seq,"num")))
			goto err;
		count = strtol(((yamldom_scalar_t*)tmp->data)->val,NULL,10);

		if(!(tmp=yamldom_find_map_val(seq,"id")))
			goto err;
		id = strtol(((yamldom_scalar_t*)tmp->data)->val,NULL,10);
		for(i=0;i<count;i++)
			INTEGER(idvec)[istart+i] = id;

		push_seq(REAL(massvec)+istart,seq,"mass",count,strtodouble);
		push_seq(REAL(intvec)+istart,seq,"intensity",count,strtodouble);
		push_seq(INTEGER(zvec)+istart,seq,"z",count,strtoint);

		istart += count;
	}

	make_list_names(df_names, ncols, "id", "mass", "intensity", "z");
	make_dataframe(df, RNULL, df_names, ncols, idvec, massvec, intvec, zvec);

	unhideGC();

	return df;

err:
	unhideGC();
	return RNULL;
}
