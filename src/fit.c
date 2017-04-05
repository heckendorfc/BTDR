#include "populate.h"
#include "utils.h"

SEXP makedf_fit(struct iobtd *iop){
	SEXP df, protidv, pidv, pindv, pcountv, pmassv, pintv, pzv, istartv, ilenv, imassv, fragv, modsv, errv;
	yamldom_node_t *fitseq, *pseq, *rseq, *iseq, *tmp, *seq;
	int i, count, peakid, protid, modstrlen;
	char id[10+10+1];
	const int ncols=13;
	char *mods;

	if(!(fitseq=yamldom_find_map_val(iop->root,"fit"))){
		goto err;
	}

	count = 0;
	for(seq=YAMLDOM_SEQ_NODES(fitseq);seq;seq=seq->next)
		count += count_seq_elem(yamldom_find_map_val(seq,"results"));

	if(count==0)
		goto err;

	hidefromGC(protidv = allocVector(INTSXP,count));
	hidefromGC(pidv = allocVector(INTSXP,count));
	hidefromGC(pindv = allocVector(INTSXP,count));
	hidefromGC(pcountv = allocVector(INTSXP,count));
	hidefromGC(pmassv = allocVector(REALSXP,count));
	hidefromGC(pintv = allocVector(REALSXP,count));
	hidefromGC(pzv = allocVector(INTSXP,count));
	hidefromGC(istartv = allocVector(INTSXP,count));
	hidefromGC(ilenv = allocVector(INTSXP,count));
	hidefromGC(imassv = allocVector(REALSXP,count));
	hidefromGC(modsv = allocVector(STRSXP,count));
	hidefromGC(fragv = allocVector(STRSXP,count));
	hidefromGC(errv = allocVector(REALSXP,count));

	i=0;
	for(fitseq=YAMLDOM_SEQ_NODES(fitseq);fitseq;fitseq=fitseq->next){
		if(!(tmp=yamldom_find_map_val(fitseq,"prot")))
			goto err;
		tmp = ((yamldom_alias_t*)tmp->data)->ref;
		push_elem(&protid,0,tmp,"id",strtoint);

		if(!(tmp=yamldom_find_map_val(tmp,"param")))
			goto err;
		tmp = ((yamldom_alias_t*)tmp->data)->ref;
		modstrlen = getmodstrlen(yamldom_find_map_val(tmp,"vmod"));
		if(!(tmp=yamldom_find_map_val(tmp,"peaks")))
			goto err;
		pseq = ((yamldom_alias_t*)tmp->data)->ref;
		push_elem(&peakid,0,pseq,"id",strtoint);

		if(!(mods=malloc(modstrlen)))
			goto err;

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
				goto err;
			iseq = ((yamldom_alias_t*)tmp->data)->ref;
			push_elem(INTEGER(istartv),i,iseq,"start",strtoint);
			push_elem(INTEGER(ilenv),i,iseq,"len",strtoint);
			push_elem(REAL(imassv),i,iseq,"mass",strtodouble);

			i++;
		}

		free(mods);
	}

	df = make_dataframe(RNULL,
						make_list_names(ncols, "protid", "peak.id", "peak.index", "peak.count", "peak.intensity", "peak.mass", "peak.z", "ion.start", "ion.len", "ion.mass", "frag", "mods", "error"),
						ncols, protidv, pidv, pindv, pcountv, pmassv, pintv, pzv, istartv, ilenv, imassv, fragv, modsv, errv);

	unhideGC();

	return df;

err:
	unhideGC();
	return RNULL;
}
