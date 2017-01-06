#ifndef __RPKG_TYPES_H
#define __RPKG_TYPES_H

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

#include "libyaml-dom/yamldom.h"

struct iobtd{
	yamldom_data_t iod;
	yamldom_node_t *root;
	yamldom_anchor_list_t anchors;
};

#endif
