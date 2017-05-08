#include "populate.h"
#include "utils.h"

void addmod(yamldom_node_t *mod, int fixed, yamldom_node_t *param, SEXP idvec, SEXP fixedvec, SEXP namevec, SEXP posvec, SEXP sitevec, SEXP massvec, int *i){
	yamldom_node_t *tmp;
	int id;

	push_elem(&id,0,param,"id",strtoint);

	while(mod){
		INTEGER(idvec)[*i] = id;

		push_elem(namevec,*i,mod,"name",strtostr);
		push_elem(INTEGER(posvec),*i,mod,"pos",strtoint);
		push_elem(INTEGER(sitevec),*i,mod,"site",strtoint);
		push_elem(REAL(massvec),*i,mod,"mass",strtodouble);

		INTEGER(fixedvec)[*i]=fixed;

		(*i)++;
		mod=mod->next;
	}
}


SEXP makedf_mod(struct iobtd *iop){
	SEXP df, idvec, fixedvec, namevec, posvec, massvec, sitevec;
	yamldom_node_t *paramseq, *tmp, *seq;
	int i, count, peakcount;
	int id;
	const int ncols=6;

	if(!(paramseq=yamldom_find_map_val(iop->root,"param"))){
		goto err;
	}

	count=0;
	for(seq=YAMLDOM_SEQ_NODES(paramseq);seq;seq=seq->next){
		if((tmp=yamldom_find_map_val(seq,"vmod")))
			count += count_seq_elem(tmp);
		if((tmp=yamldom_find_map_val(seq,"fmod")))
			count += count_seq_elem(tmp);
	}

	if(count==0)
		goto err;

	hidefromGC(idvec = allocVector(INTSXP,count));
	hidefromGC(fixedvec = allocVector(INTSXP,count));
	hidefromGC(namevec = allocVector(STRSXP,count));
	hidefromGC(posvec = allocVector(INTSXP,count));
	hidefromGC(sitevec = allocVector(INTSXP,count));
	hidefromGC(massvec = allocVector(REALSXP,count));

	i=0;
	for(seq=YAMLDOM_SEQ_NODES(paramseq);seq;seq=seq->next){
		if((tmp=yamldom_find_map_val(seq,"fmod")))
			addmod(YAMLDOM_SEQ_NODES(tmp),1,seq,idvec,fixedvec,namevec,posvec,sitevec,massvec,&i);
		if((tmp=yamldom_find_map_val(seq,"vmod")))
			addmod(YAMLDOM_SEQ_NODES(tmp),0,seq,idvec,fixedvec,namevec,posvec,sitevec,massvec,&i);
	}

	df = make_dataframe(RNULL,
						make_list_names(ncols, "paramid", "fixed", "name", "mass", "pos", "site"),
						ncols, idvec, fixedvec, namevec, massvec, posvec, sitevec);

	unhideGC();

	return df;

err:
	unhideGC();
	return RNULL;
}
