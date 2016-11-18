#include <stdlib.h>
#include <string.h>

#include "populate.h"

void strtoint(char *str, void *t, int i){
	((int*)t)[i]=strtol(str,NULL,10);
}

void strtodouble(char *str, void *t, int i){
	((double*)t)[i]=strtod(str,NULL);
}

void strtostr(char *str, void *t, int i){
	SET_STRING_ELT(t, i, mkChar(str));
}

void push_elem(void *vec, int i, yamldom_node_t *root, char *name, void(*tonum)(char*,void*,int)){
	yamldom_node_t *tmp;
	if(!(tmp=yamldom_find_map_val(root,name))){
		tonum("0",vec,i);
	} else
		tonum(((yamldom_scalar_t*)tmp->data)->val,vec,i);
}

void push_elem_offset(void *vec, int i, int off, yamldom_node_t *root, char *name, void(*tonum)(char*,void*,int)){
	yamldom_node_t *tmp;
	if(!(tmp=yamldom_find_map_val(root,name))){
		tonum("0",vec,i);
	} else {
		tmp = YAMLDOM_SEQ_NODES(tmp);
		while(off>0 && tmp){
			tmp=tmp->next;
			off--;
		}
		tonum(((yamldom_scalar_t*)tmp->data)->val,vec,i);
	}
}

void push_seq(void *vec, yamldom_node_t *root, char *name, int num, void(*tonum)(char*,void*,int)){
	int i;
	yamldom_node_t *tmp;

	if(!(tmp=yamldom_find_map_val(root,name))){
		for(i=0;i<num;i++)
			tonum("0",vec,i);
		return;
	}

	tmp = YAMLDOM_SEQ_NODES(tmp);
	for(i=0;i<num && tmp;i++){
		tonum(((yamldom_scalar_t*)tmp->data)->val,vec,i);
		tmp=tmp->next;
	}
	for(;i<num;i++)
		tonum("0",vec,i);
}

int count_seq_elem(yamldom_node_t *seq){
	yamldom_node_t *tmp;
	int count=0;

	if(!seq)
		return 0;

	for(tmp=YAMLDOM_SEQ_NODES(seq);tmp;tmp=tmp->next)
		count++;

	return count;
}

void set_searchid(yamldom_node_t *searchseq, char *id, int *peakidp, int *protidp){
	int protid, peakid;
	yamldom_node_t *tmp, *protseq;

	if(!(protseq=yamldom_find_map_val(searchseq,"prot")))
		return;
	tmp = ((yamldom_alias_t*)protseq->data)->ref;
	push_elem(&protid,0,tmp,"id",strtoint);

	if(!(tmp=yamldom_find_map_val(tmp,"param")))
		return;
	if(!(tmp=yamldom_find_map_val(((yamldom_alias_t*)tmp->data)->ref,"peaks")))
		return;
	push_elem(&peakid,0,((yamldom_alias_t*)tmp->data)->ref,"id",strtoint);

	if(id)
		sprintf(id,"%d-%d",peakid,protid);

	if(peakidp)
		*peakidp = peakid;
	if(protidp)
		*protidp = protid;
}

