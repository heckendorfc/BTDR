#ifndef YAMLDOM_H
#define YAMLDOM_H

#include <stdio.h>
#include <yaml.h>

enum y_types{
	Y_SEQ,
	Y_MAP,
	Y_SCALAR,
	Y_ALIAS,
};

#define YAMLDOM_MAP_NODES(x) (((yamldom_map_t*)x->data)->nodes)
#define YAMLDOM_SEQ_NODES(x) (((yamldom_seq_t*)x->data)->nodes)
#define YAMLDOM_SCALAR_DATA(x) ((yamldom_scalar_t*)x->data)
#define YAMLDOM_ALIAS_DATA(x) ((yamldom_alias_t*)x->data)

typedef struct yamldom_node_s{
	struct yamldom_node_s *next;
	int type;
	char *anchor;
	void *data;
}yamldom_node_t;

typedef struct yamldom_scalar_s{
	char *val;
	char *tag;
}yamldom_scalar_t;

typedef struct yamldom_alias_s{
	char *val;
	yamldom_node_t *ref;
}yamldom_alias_t;

typedef struct yamldom_seq_s{
	yamldom_node_t *nodes;
}yamldom_seq_t;

typedef struct yamldom_map_s{
	yamldom_node_t *nodes; //key_a->val_a->key_b->...
}yamldom_map_t;

typedef struct yamldom_anchor_list_s{
	struct yamldom_anchor_list_s *next;
	char *val;
	yamldom_node_t *ref;
}yamldom_anchor_list_t;

typedef struct yamldom_data_t{
	FILE *infd;
	FILE *outfd;
	yaml_emitter_t emitter;
	yaml_parser_t parser;
	yaml_event_t event;
}yamldom_data_t;

/* gen */
yamldom_node_t* yamldom_append_node(yamldom_node_t *a, yamldom_node_t *b);
yamldom_node_t* yamldom_make_seq(char *anchor);
yamldom_node_t* yamldom_make_map(char *anchor);
yamldom_node_t* yamldom_find_anchor_node(yamldom_anchor_list_t *anchors, const char *alias);
yamldom_node_t* yamldom_make_scalar(char *tag, char *val, size_t length);
yamldom_node_t* yamldom_make_scalar_alloc(char *tag, char *val, size_t length);
yamldom_node_t* yamldom_make_alias(yamldom_anchor_list_t *anchors, char *alias);
yamldom_node_t* yamldom_find_map_val(yamldom_node_t *root, const char *name);
yamldom_node_t* yamldom_make_map_nextanchor(yamldom_node_t *seqroot, const char *fmt);
yamldom_node_t* yamldom_gen(yamldom_data_t *ydd, yamldom_anchor_list_t *anchor_ret);

/* dump */
void yamldom_dump(yamldom_data_t *ydd, yamldom_node_t *root);

/* clean */
void yamldom_free_nodes(yamldom_node_t *root);
void yamldom_free_anchors(yamldom_anchor_list_t *root);

#endif
