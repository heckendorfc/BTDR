/* Copyright 2014, Heckendorf */

#include <string.h>
#include <yaml.h>
#include "yamldom.h"
#include "list.h"
#include "common.h"

yamldom_node_t* yamldom_append_node(yamldom_node_t *a, yamldom_node_t *b){
	yamldom_node_t *ret=a;

	if(!a)
		return b;

	while(a->next)a=a->next;
	a->next=b;

	return ret;
}

yamldom_node_t* yamldom_make_seq(char *anchor){
	yamldom_node_t *tmpnode=NULL;
	yamldom_seq_t *data;

	INIT_MEM(data,1);
	data->nodes=NULL;

	INIT_MEM(tmpnode,1);
	tmpnode->type=Y_SEQ;
	tmpnode->data=data;
	tmpnode->anchor=anchor?strdup(anchor):NULL;
	tmpnode->next=NULL;

	return tmpnode;
}

yamldom_node_t* yamldom_make_map(char *anchor){
	yamldom_node_t *tmpnode=NULL;
	yamldom_map_t *data;

	INIT_MEM(data,1);
	data->nodes=NULL;

	INIT_MEM(tmpnode,1);
	tmpnode->type=Y_MAP;
	tmpnode->data=data;
	tmpnode->anchor=anchor?strdup(anchor):NULL;
	tmpnode->next=NULL;

	return tmpnode;
}

yamldom_node_t* yamldom_find_anchor_node(yamldom_anchor_list_t *anchors, const char *alias){
	while(anchors){
		if(anchors->val && strcmp(anchors->val,alias)==0)
			return anchors->ref;
		anchors=anchors->next;
	}
	return NULL;
}

yamldom_node_t* yamldom_make_scalar(char *tag, char *val, size_t length){
	yamldom_node_t *tmpnode=NULL;
	yamldom_scalar_t *data;

	INIT_MEM(data,1);
	data->val=val?strdup(val):NULL;
	data->tag=tag?strdup(tag):NULL;

	INIT_MEM(tmpnode,1);
	tmpnode->type=Y_SCALAR;
	tmpnode->anchor=NULL;
	tmpnode->data=data;
	tmpnode->next=NULL;

	return tmpnode;
}

yamldom_node_t* yamldom_make_scalar_alloc(char *tag, char *val, size_t length){
	yamldom_node_t *node=yamldom_make_scalar(tag,NULL,-1);
	((yamldom_scalar_t*)node->data)->val=val;
	return node;
}

yamldom_node_t* yamldom_make_alias(yamldom_anchor_list_t *anchors, char *alias){
	yamldom_node_t *tmpnode=NULL;
	yamldom_alias_t *data;

	INIT_MEM(data,1);
	data->val=strdup(alias);
	data->ref=yamldom_find_anchor_node(anchors,alias);

	INIT_MEM(tmpnode,1);
	tmpnode->type=Y_ALIAS;
	tmpnode->anchor=NULL;
	tmpnode->data=data;
	tmpnode->next=NULL;

	return tmpnode;
}

static yamldom_node_t* io_gen_rec(yamldom_data_t *ydd, yamldom_anchor_list_t *anchors, int end_type){
	yaml_event_t  event;
	yamldom_node_t *curnode=NULL,*nodes=NULL;
	yamldom_anchor_list_t *tmp,*anchor_tail=anchors;

	do {
		if (!yaml_parser_parse(&ydd->parser, &event)) {
			printf("Parser error %d\n", ydd->parser.error);
			exit(EXIT_FAILURE);
		}

		if(event.type==end_type)
			return nodes;

		switch(event.type)
		{
			case YAML_NO_EVENT: puts("No event!"); break;

			case YAML_STREAM_START_EVENT:
			case YAML_STREAM_END_EVENT:

			case YAML_DOCUMENT_START_EVENT:
			case YAML_DOCUMENT_END_EVENT:
				break;

			case YAML_SEQUENCE_START_EVENT:
				curnode=yamldom_make_seq(event.data.sequence_start.anchor);
				if(event.data.sequence_start.anchor){
					FINDTAILNODE(anchor_tail);
					ADDNODEAFTER(anchor_tail,tmp);
					anchor_tail->next->ref=curnode;
					anchor_tail->next->val=strdup(event.data.sequence_start.anchor);
				}
				nodes=yamldom_append_node(nodes,curnode);
				((yamldom_seq_t*)curnode->data)->nodes=io_gen_rec(ydd,anchors,YAML_SEQUENCE_END_EVENT);
				break;
			case YAML_MAPPING_START_EVENT:
				curnode=yamldom_make_map(event.data.mapping_start.anchor);
				if(event.data.mapping_start.anchor){
					FINDTAILNODE(anchor_tail);
					ADDNODEAFTER(anchor_tail,tmp);
					anchor_tail->next->ref=curnode;
					anchor_tail->next->val=strdup(event.data.mapping_start.anchor);
				}
				nodes=yamldom_append_node(nodes,curnode);
				((yamldom_map_t*)curnode->data)->nodes=io_gen_rec(ydd,anchors,YAML_MAPPING_END_EVENT);
				break;

			case YAML_ALIAS_EVENT:
				curnode=yamldom_make_alias(anchors,event.data.alias.anchor);
				nodes=yamldom_append_node(nodes,curnode);
				break;
			case YAML_SCALAR_EVENT:
				curnode=yamldom_make_scalar(event.data.scalar.tag,event.data.scalar.value,event.data.scalar.length);
				nodes=yamldom_append_node(nodes,curnode);
				break;
		}
		if(event.type != YAML_STREAM_END_EVENT)
			yaml_event_delete(&event);
	} while(event.type != YAML_STREAM_END_EVENT);
	yaml_event_delete(&event);

	return NULL;
}

yamldom_node_t* yamldom_gen(yamldom_data_t *ydd, yamldom_anchor_list_t *anchor_ret){
	yamldom_node_t *node_ret;
	yamldom_anchor_list_t anchors;

	anchors.val=NULL;
	anchors.ref=NULL;
	anchors.next=NULL;

	node_ret=io_gen_rec(ydd,&anchors,YAML_STREAM_END_EVENT);

	if(anchor_ret){
		anchor_ret->next=anchors.next;
		anchor_ret->val=NULL;
	}

	return node_ret;
}

yamldom_node_t* yamldom_find_map_val(yamldom_node_t *root, const char *name){
	yamldom_node_t *node;

	node=YAMLDOM_MAP_NODES(root);

	while(node){
		if(node->type==Y_SCALAR && strcmp(((yamldom_scalar_t*)node->data)->val,name)==0)
			return node->next;
		node=node->next;
	}

	return NULL;
}

yamldom_node_t* yamldom_make_map_nextanchor(yamldom_node_t *seqroot, const char *fmt){
	yamldom_node_t *cnode=YAMLDOM_SEQ_NODES(seqroot);
	char buf[100];
	int max=1;
	int cur;

	while(cnode){
		sscanf(cnode->anchor,fmt,&cur);
		if(cur>=max)
			max=cur+1;
		cnode=cnode->next;
	}

	sprintf(buf,fmt,max);
	return yamldom_make_map(buf);
}
