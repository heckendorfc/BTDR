#ifndef __RPKG_UTILS_H__
#define __RPKG_UTILS_H__

void strtoint(char *str, void *t, int i);
void strtodouble(char *str, void *t, int i);
void strtostr(char *str, void *t, int i);
void push_elem(void *vec, int i, yamldom_node_t *root, char *name, void(*tonum)(char*,void*,int));
void push_elem_offset(void *vec, int i, int off, yamldom_node_t *root, char *name, void(*tonum)(char*,void*,int));
void push_seq(void *vec, yamldom_node_t *root, char *name, int num, void(*tonum)(char*,void*,int));
int count_seq_elem(yamldom_node_t *seq);

void set_searchid(yamldom_node_t *searchseq, char *id, int *peakidp, int *protidp);

int getmodstrlen(yamldom_node_t *mods);
void modstr(yamldom_node_t *mods, char *str);

#endif
