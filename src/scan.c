#include "populate.h"
#include "utils.h"

SEXP makedf_scan(struct iobtd *iop){
	SEXP df, plidvec, sidvec, mzvec, zvec, intvec, rtvec;
	yamldom_node_t *peakseq, *scanseq, *tmp, *seq, *scan;
	int i, count, peakcount;
	int id;
	const int ncols=6;

	if(!(scanseq=yamldom_find_map_val(iop->root,"scan"))){
		goto err;
	}

	if(!(peakseq=yamldom_find_map_val(iop->root,"peaks"))){
		goto err;
	}

	count=0;
	for(seq=YAMLDOM_SEQ_NODES(peakseq);seq;seq=seq->next){
		if(!(tmp=yamldom_find_map_val(seq,"scans")))
			goto err;
		count += count_seq_elem(tmp);
	}

	hidefromGC(sidvec = allocVector(INTSXP,count));
	hidefromGC(plidvec = allocVector(INTSXP,count));
	hidefromGC(mzvec = allocVector(REALSXP,count));
	hidefromGC(zvec = allocVector(INTSXP,count));
	hidefromGC(rtvec = allocVector(REALSXP,count));
	hidefromGC(intvec = allocVector(REALSXP,count));

	i=0;
	for(seq=YAMLDOM_SEQ_NODES(peakseq);seq;seq=seq->next){
		if(!(tmp=yamldom_find_map_val(seq,"id")))
			goto err;
		id=strtol(((yamldom_scalar_t*)tmp->data)->val,NULL,10);

		if(!(scanseq=yamldom_find_map_val(seq,"scans")))
			goto err;

		for(tmp=YAMLDOM_SEQ_NODES(scanseq);tmp;tmp=tmp->next){
			scan=((yamldom_alias_t*)tmp->data)->ref;

			INTEGER(plidvec)[i]=id;
			push_elem(INTEGER(sidvec),i,scan,"id",strtoint);
			push_elem(INTEGER(zvec),i,scan,"z",strtoint);
			push_elem(REAL(mzvec),i,scan,"mz",strtodouble);
			push_elem(REAL(intvec),i,scan,"preint",strtodouble);
			push_elem(REAL(rtvec),i,scan,"rt",strtodouble);
			if(REAL(rtvec)[i]<0.)
				REAL(rtvec)[i]=0.;

			scanseq = scanseq->next;
			i++;
		}
	}

	df = make_dataframe(RNULL,
						make_list_names(ncols, "plid", "scanid", "mz", "z", "rt", "pre.int"),
						ncols, plidvec, sidvec, mzvec, zvec, rtvec, intvec);

	unhideGC();

	return df;

err:
	unhideGC();
	return RNULL;
}
