#include "populate.h"
#include "utils.h"

SEXP makedf_prot(struct iobtd *iop){
	SEXP df, protidvec, peakidvec, seqvec, namevec, startvec, lenvec;
	yamldom_node_t *protseq, *tmp, *seq;
	int i, len, count, peakcount;
	int id;
	char *name;
	const int ncols=6;

	if(!(protseq=yamldom_find_map_val(iop->root,"prot"))){
		return RNULL;
	}

	count = count_seq_elem(protseq);

	if(count==0)
		return RNULL;

	hidefromGC(protidvec = allocVector(INTSXP,count));
	hidefromGC(peakidvec = allocVector(INTSXP,count));
	hidefromGC(seqvec = allocVector(STRSXP,count));
	hidefromGC(namevec = allocVector(STRSXP,count));
	hidefromGC(startvec = allocVector(INTSXP,count));
	hidefromGC(lenvec = allocVector(INTSXP,count));

	i=0;
	for(seq=YAMLDOM_SEQ_NODES(protseq);seq;seq=seq->next){
		if(!(tmp=yamldom_find_map_val(seq,"param")))
			return RNULL;
		if(!(tmp=yamldom_find_map_val(((yamldom_alias_t*)tmp->data)->ref,"peaks")))
			return RNULL;
		push_elem(INTEGER(peakidvec),i,((yamldom_alias_t*)tmp->data)->ref,"id",strtoint);

		push_elem(INTEGER(protidvec),i,seq,"id",strtoint);
		push_elem(seqvec,i,seq,"seq",strtostr);
		push_elem(namevec,i,seq,"name",strtostr);
		push_elem(INTEGER(startvec),i,seq,"start",strtoint);
		push_elem(INTEGER(lenvec),i,seq,"len",strtoint);

		if(INTEGER(lenvec)[i]==0){ /* Legacy support :( */
			INTEGER(lenvec)[i] = strlen(CHARPT(seqvec,i));
			name = CHARPT(namevec,i);
			len = strlen(name);
			if(name[len-1] == ']'){
				for(len--;len>0 && name[len-1]!='[';len--)
					;
				if(len>0 && name[len-1]=='['){
					INTEGER(startvec)[i] = strtol(name+len,NULL,10);
					name[len]=0;
				}
			}
		}
		i++;
	}

	hidefromGC(df = make_dataframe(RNULL,
								make_list_names(ncols, "protid", "peakid", "seq", "name", "start", "len"),
								ncols, protidvec, peakidvec, seqvec, namevec, startvec, lenvec));

	return df;
}
