#include "populate.h"
#include "utils.h"

SEXP makedf_tag(struct iobtd *iop){
	SEXP df, idvec, startvec, lenvec;
	yamldom_node_t *searchseq, *tmp, *seq;
	int i, count;
	char id[10+10+1];
	const int ncols=3;

	if(!(searchseq=yamldom_find_map_val(iop->root,"search"))){
		goto err;
	}

	count = 0;
	for(seq=YAMLDOM_SEQ_NODES(searchseq);seq;seq=seq->next)
		count += count_seq_elem(yamldom_find_map_val(seq,"tags"));

	if(count==0)
		goto err;

	hidefromGC(idvec = allocVector(STRSXP,count));
	hidefromGC(startvec = allocVector(INTSXP,count));
	hidefromGC(lenvec = allocVector(INTSXP,count));

	i=0;
	for(seq=YAMLDOM_SEQ_NODES(searchseq);seq;seq=seq->next){
		set_searchid(seq,id,NULL,NULL);

		if(!(tmp=yamldom_find_map_val(seq,"tags")))
			goto err;

		for(tmp=YAMLDOM_SEQ_NODES(tmp);tmp;tmp=tmp->next){
			SET_STRING_ELT(idvec, i, mkChar(id));

			push_elem(INTEGER(startvec),i,tmp,"start",strtoint);
			push_elem(INTEGER(lenvec),i,tmp,"len",strtoint);
			i++;
		}
	}

	df = make_dataframe(RNULL,
						make_list_names(ncols, "searchid", "start", "len"),
						ncols, idvec, startvec, lenvec);

	unhideGC();

	return df;

err:
	unhideGC();
	return RNULL;
}
