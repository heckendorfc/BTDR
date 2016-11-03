#include "populate.h"
#include "utils.h"

void addmod(yamldom_node_t *mod, int fixed, yamldom_node_t *param, SEXP idvec, SEXP fixedvec, SEXP namevec, SEXP posvec, SEXP sitevec, SEXP massvec, int *i){
	yamldom_node_t *tmp;

	while(mod){
		if(!(tmp=yamldom_find_map_val(param,"peaks")))
			return;
		push_elem(INTEGER(idvec),*i,((yamldom_alias_t*)tmp->data)->ref,"id",strtoint);

		push_elem(namevec,*i,mod,"name",strtostr);
		push_elem(INTEGER(posvec),*i,mod,"pos",strtoint);
		push_elem(INTEGER(sitevec),*i,mod,"site",strtoint);
		push_elem(REAL(massvec),*i,mod,"mass",strtodouble);

		(*i)++;
		mod=mod->next;
	}
}


SEXP makedf_mod(struct iobtd *iop){
	SEXP df, idvec, fixedvec, namevec, posvec, massvec, sitevec;
	yamldom_node_t *paramseq, *tmp, *seq;
	int i, istart, count, peakcount;
	int id;
	const int ncols=6;

	if(!(paramseq=yamldom_find_map_val(iop->root,"param"))){
		return RNULL;
	}

	count=0;
	for(seq=YAMLDOM_SEQ_NODES(paramseq);seq;seq=seq->next){
		if((tmp=yamldom_find_map_val(seq,"vmod")))
			count += count_seq_elem(tmp);
		if((tmp=yamldom_find_map_val(seq,"fmod")))
			count += count_seq_elem(tmp);
	}

	if(count==0)
		return RNULL;

	PROTECT(idvec = allocVector(INTSXP,count));
	PROTECT(fixedvec = allocVector(INTSXP,count));
	PROTECT(namevec = allocVector(STRSXP,count));
	PROTECT(posvec = allocVector(INTSXP,count));
	PROTECT(sitevec = allocVector(INTSXP,count));
	PROTECT(massvec = allocVector(REALSXP,count));

	istart=i=0;
	for(seq=YAMLDOM_SEQ_NODES(paramseq);seq;seq=seq->next){
		if((tmp=yamldom_find_map_val(seq,"fmod")))
			addmod(YAMLDOM_SEQ_NODES(tmp),1,seq,idvec,fixedvec,namevec,posvec,sitevec,massvec,&i);
		if((tmp=yamldom_find_map_val(seq,"vmod")))
			addmod(YAMLDOM_SEQ_NODES(tmp),0,seq,idvec,fixedvec,namevec,posvec,sitevec,massvec,&i);
	}

	PROTECT(df = make_dataframe(RNULL,
								make_list_names(ncols, "paramid", "fixed", "name", "mass", "pos", "site"),
								ncols, idvec, fixedvec, namevec, massvec, posvec, sitevec));

	UNPROTECT(7);

	return df;
}
