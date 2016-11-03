#include "populate.h"
#include "utils.h"

int getmodstrlen(yamldom_node_t *mods){
	int ret=0;
	yamldom_node_t *tmp;

	for(mods=YAMLDOM_SEQ_NODES(mods);mods;mods=mods->next){
		if((tmp=yamldom_find_map_val(mods,"name")))
			ret += strlen(((yamldom_alias_t*)tmp->data)->val);
	}

	return ret;
}

void modstr(yamldom_node_t *mods, char *str){
	yamldom_node_t *tmp;
	char *tmod;
	int num;

	str[0]=0;

	for(mods=YAMLDOM_SEQ_NODES(mods);mods;mods=mods->next){
		if(!(tmp=yamldom_find_map_val(mods,"num")))
			return;
		push_elem(&num,0,tmp,"num",strtoint);

		if(!(tmp=yamldom_find_map_val(mods,"mod")))
			return;
		tmp = ((yamldom_alias_t*)tmp->data)->ref;
		if(!(tmp=yamldom_find_map_val(mods,"name")))
		tmp = ((yamldom_alias_t*)tmp->data)->ref;

		asprintf(&tmod,"%dx %s",num,((yamldom_scalar_t*)tmp->data)->val);

		if(*str)
			strcat(str,", ");
		strcat(str,tmod);
	}
}

SEXP makedf_fit(struct iobtd *iop){
	SEXP df, protidv, pidv, pindv, pcountv, pmassv, pintv, pzv, istartv, ilenv, imassv, fragv, modsv, errv;
	yamldom_node_t *fitseq, *pseq, *rseq, *iseq, *tmp, *seq;
	int i, count, peakid, protid, modstrlen;
	char id[10+10+1];
	const int ncols=13;
	char *mods;

	if(!(fitseq=yamldom_find_map_val(iop->root,"fit"))){
		return RNULL;
	}

	count = 0;
	for(seq=YAMLDOM_SEQ_NODES(fitseq);seq;seq=seq->next)
		count += count_seq_elem(yamldom_find_map_val(seq,"results"));

	if(count==0)
		return RNULL;

	PROTECT(protidv = allocVector(INTSXP,count));
	PROTECT(pidv = allocVector(INTSXP,count));
	PROTECT(pindv = allocVector(INTSXP,count));
	PROTECT(pcountv = allocVector(INTSXP,count));
	PROTECT(pmassv = allocVector(REALSXP,count));
	PROTECT(pintv = allocVector(REALSXP,count));
	PROTECT(pzv = allocVector(INTSXP,count));
	PROTECT(istartv = allocVector(INTSXP,count));
	PROTECT(ilenv = allocVector(INTSXP,count));
	PROTECT(imassv = allocVector(REALSXP,count));
	PROTECT(modsv = allocVector(STRSXP,count));
	PROTECT(fragv = allocVector(STRSXP,count));
	PROTECT(errv = allocVector(REALSXP,count));

	i=0;
	for(fitseq=YAMLDOM_SEQ_NODES(fitseq);fitseq;fitseq=fitseq->next){
		if(!(tmp=yamldom_find_map_val(fitseq,"prot")))
			return RNULL;
		tmp = ((yamldom_alias_t*)tmp->data)->ref;
		push_elem(&protid,0,tmp,"id",strtoint);

		if(!(tmp=yamldom_find_map_val(tmp,"param")))
			return RNULL;
		tmp = ((yamldom_alias_t*)tmp->data)->ref;
		modstrlen = getmodstrlen(yamldom_find_map_val(tmp,"vmod"));
		if(!(tmp=yamldom_find_map_val(tmp,"peaks")))
			return RNULL;
		pseq = ((yamldom_alias_t*)tmp->data)->ref;
		push_elem(&peakid,0,pseq,"id",strtoint);

		if(!(mods=malloc(modstrlen)))
			return RNULL;

		for(seq=YAMLDOM_SEQ_NODES(yamldom_find_map_val(fitseq,"results"));seq;seq=seq->next){
			rseq=seq;
			INTEGER(protidv)[i] = protid;
			INTEGER(pidv)[i] = peakid;

			push_elem(INTEGER(pindv),i,rseq,"peak",strtoint);
			push_elem(INTEGER(pcountv),i,rseq,"peakcount",strtoint);
			push_elem(REAL(errv),i,rseq,"err",strtodouble);
			push_elem(fragv,i,rseq,"frag",strtostr);
			modstr(yamldom_find_map_val(rseq,"mods"),mods);
			if(mods && *mods){
				SET_STRING_ELT(modsv, i, mkChar(mods));
				free(mods);
			} else
				SET_STRING_ELT(modsv, i, mkChar(""));

			push_elem_offset(REAL(pmassv),i,INTEGER(pindv)[i],pseq,"mass",strtodouble);
			push_elem_offset(REAL(pintv),i,INTEGER(pindv)[i],pseq,"intensity",strtodouble);
			push_elem_offset(INTEGER(pzv),i,INTEGER(pindv)[i],pseq,"z",strtoint);

			if(!(tmp=yamldom_find_map_val(rseq,"ion")))
				return RNULL;
			iseq = ((yamldom_alias_t*)tmp->data)->ref;
			push_elem(INTEGER(istartv),i,iseq,"start",strtoint);
			push_elem(INTEGER(ilenv),i,iseq,"len",strtoint);
			push_elem(REAL(imassv),i,iseq,"mass",strtodouble);

			i++;
		}

		free(mods);
	}

	PROTECT(df = make_dataframe(RNULL,
								make_list_names(ncols, "protid", "peak.id", "peak.index", "peak.count", "peak.intensity", "peak.mass", "peak.z", "ion.start", "ion.len", "ion.mass", "frag", "mods", "error"),
								ncols, protidv, pidv, pindv, pcountv, pmassv, pintv, pzv, istartv, ilenv, imassv, fragv, modsv, errv));

	UNPROTECT(14);

	return df;
}
