#include "populate.h"
#include "utils.h"

SEXP makedf_xlink(struct iobtd *iop){
	SEXP df, idvec, peakidvec, errvec, covvec, modsv;
	yamldom_node_t *xlseq, *tmp, *seq, *prottmp, *peptmp;
	int i, istart, count, peakcount, modstrlen;
	int id;
	double pre_err, frag_cov;
	const int ncols=5;
	char *mods;

	if(!(xlseq=yamldom_find_map_val(iop->root,"xlink"))){
		goto err;
	}

	count = count_seq_elem(xlseq);

	hidefromGC(idvec = allocVector(INTSXP,count));
	hidefromGC(peakidvec = allocVector(INTSXP,count));
	hidefromGC(errvec = allocVector(REALSXP,count));
	hidefromGC(covvec = allocVector(REALSXP,count));
	hidefromGC(modsv = allocVector(STRSXP,count));

	istart=i=0;
	for(seq=YAMLDOM_SEQ_NODES(xlseq);seq;seq=seq->next){
		if(!(peptmp=yamldom_find_map_val(seq,"peps")))
			goto err;
		peptmp = YAMLDOM_SEQ_NODES(peptmp);

		push_elem(INTEGER(idvec),i,seq,"id",strtoint);
		push_elem(REAL(errvec),i,seq,"error",strtodouble);
		push_elem(REAL(covvec),i,seq,"fragcov",strtodouble);

		/* peaklist id (via prot -> param) */
		if(!(prottmp=yamldom_find_map_val(peptmp,"prot")))
			goto err;
		tmp = YAMLDOM_DEREF(prottmp);
		if(!(tmp=yamldom_find_map_val(tmp,"param")))
			goto err;
		tmp = YAMLDOM_DEREF(tmp);
		modstrlen = getmodstrlen(yamldom_find_map_val(tmp,"vmod"));
		if(!(tmp=yamldom_find_map_val(tmp,"peaks")))
			goto err;
		push_elem(INTEGER(peakidvec),i,YAMLDOM_DEREF(tmp),"id",strtoint);

		if(!(mods=malloc(modstrlen)))
			goto err;

		modstr(yamldom_find_map_val(seq,"mods"),mods);
		if(mods && *mods){
			SET_STRING_ELT(modsv, i, mkChar(mods));
			free(mods);
		} else
			SET_STRING_ELT(modsv, i, mkChar(""));

		i++;
	}

	df = make_dataframe(RNULL,
						make_list_names(ncols, "xlid", "peakid", "error", "frag.cov", "mods"),
						ncols, idvec, peakidvec, errvec, covvec, modsv);

	unhideGC();

	return df;

err:
	unhideGC();
	return RNULL;
}


SEXP makedf_xlpep(struct iobtd *iop){
	SEXP df, idvec, protidvec, pepflagvec, pepstartvec, peplenvec, pepmassvec;
	yamldom_node_t *xlseq, *tmp, *seq, *prottmp, *peptmp;
	int i, istart, count, peakcount, modstrlen;
	int id;
	double pre_err, frag_cov;
	const int ncols=6;

	if(!(xlseq=yamldom_find_map_val(iop->root,"xlink"))){
		goto err;
	}

	count = 0;
	for(seq=YAMLDOM_SEQ_NODES(xlseq);seq;seq=seq->next)
		count += count_seq_elem(yamldom_find_map_val(seq,"peps"));

	hidefromGC(idvec = allocVector(INTSXP,count));
	hidefromGC(protidvec = allocVector(INTSXP,count));
	hidefromGC(pepflagvec = allocVector(INTSXP,count));
	hidefromGC(pepstartvec = allocVector(INTSXP,count));
	hidefromGC(peplenvec = allocVector(INTSXP,count));
	hidefromGC(pepmassvec = allocVector(REALSXP,count));

	istart=i=0;
	for(seq=YAMLDOM_SEQ_NODES(xlseq);seq;seq=seq->next){
		if(!(peptmp=yamldom_find_map_val(seq,"peps")))
			goto err;
		peptmp = YAMLDOM_SEQ_NODES(peptmp);

		push_elem(&id,0,seq,"id",strtoint);

		while(peptmp){
			INTEGER(idvec)[i] = id;

			/* prot id/rank (via prot) */
			if(!(prottmp=yamldom_find_map_val(peptmp,"prot")))
				goto err;
			tmp = YAMLDOM_DEREF(prottmp);
			push_elem(INTEGER(protidvec),i,tmp,"id",strtoint);

			push_elem(INTEGER(pepflagvec),i,peptmp,"flags",strtoint);
			push_elem(INTEGER(pepstartvec),i,peptmp,"start",strtoint);
			push_elem(INTEGER(peplenvec),i,peptmp,"len",strtoint);
			push_elem(REAL(pepmassvec),i,peptmp,"mass",strtodouble);

			i++;
			peptmp = peptmp->next;
		}
	}

	df = make_dataframe(RNULL,
						make_list_names(ncols, "xlid", "protid", "pep.flag", "pep.start", "pep.len", "pep.mass"),
						ncols, idvec, protidvec, pepflagvec, pepstartvec, peplenvec, pepmassvec);

	unhideGC();

	return df;

err:
	unhideGC();
	return RNULL;
}


