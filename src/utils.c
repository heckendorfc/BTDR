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
	tmp = YAMLDOM_DEREF(protseq);
	push_elem(&protid,0,tmp,"id",strtoint);

	if(!(tmp=yamldom_find_map_val(tmp,"peaks")))
		return;
	push_elem(&peakid,0,YAMLDOM_DEREF(tmp),"id",strtoint);

	if(id)
		sprintf(id,"%d-%d",peakid,protid);

	if(peakidp)
		*peakidp = peakid;
	if(protidp)
		*protidp = protid;
}

int getmodstrlen(yamldom_node_t *mods){
	int ret=0;
	yamldom_node_t *tmp;

	for(mods=YAMLDOM_SEQ_NODES(mods);mods;mods=mods->next){
		if((tmp=yamldom_find_map_val(mods,"name")))
			ret += 10+2+strlen(((yamldom_alias_t*)tmp->data)->val);
	}

	return ret;
}

void modstr(yamldom_node_t *mods, char *str){
	yamldom_node_t *tmp;
	char *tmod;
	char *p;
	int num;
	int len;

	str[0]=0;

	if(mods==NULL)
		return;

	for(mods=YAMLDOM_SEQ_NODES(mods);mods;mods=mods->next){
		if(!(tmp=yamldom_find_map_val(mods,"num")))
			return;
		push_elem(&num,0,mods,"num",strtoint);

		if(!(tmp=yamldom_find_map_val(mods,"mod")))
			return;
		tmp = ((yamldom_alias_t*)tmp->data)->ref;
		if(!(tmp=yamldom_find_map_val(tmp,"name")))
			return;

		p=((yamldom_scalar_t*)tmp->data)->val;
		len=10+2+strlen(p)+1;
		tmod = malloc(len);
		sprintf(tmod,"%dx %s",num,p);

		if(*str)
			strcat(str,", ");
		strcat(str,tmod);
		free(tmod);
	}
}

