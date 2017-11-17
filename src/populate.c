#include <unistd.h>
#include <stdio.h>

#include "populate.h"

int io_general_init(yamldom_data_t *iod){
	memset(&iod->parser,0,sizeof(iod->event));

	if(iod->infd){
		if(!yaml_parser_initialize(&iod->parser))
			goto error;

		yaml_parser_set_input_file(&iod->parser, iod->infd);
		yaml_parser_set_encoding(&iod->parser,YAML_UTF8_ENCODING);
	}

	return 0;

error:
	return 1;
}

int io_general_close(yamldom_data_t *iod){
	if(iod->infd)
		yaml_parser_delete(&iod->parser);

	return 0;
}

int find_anchor_id(yaml_document_t *doc, const char *name, int index){
	int r;
	yaml_node_t *node;

	for(r=1;(node=yaml_document_get_node(doc,r));r++){
		if(node->type!=YAML_SCALAR_NODE)
			continue;
	}

	return 0;
}

int io_init(struct iobtd *iop, FILE *in){
	iop->iod.infd=in;
	iop->iod.outfd=NULL;

	io_general_init(&iop->iod);

	iop->root=yamldom_gen(&iop->iod,&iop->anchors);

	return 0;
}

int io_close(struct iobtd *iop){
	io_general_close(&iop->iod);

	fclose(iop->iod.infd);

	yamldom_free_nodes(iop->root);
	yamldom_free_anchors(iop->anchors.next);

	return 0;
}

#define safedf(dst,df){ \
	tmp = df; \
	if(tmp != RNULL){ \
		hidefromGC(tmp); \
		dst = tmp; \
	} else { \
		make_dataframe(dst,RNULL,RNULL,0,0); \
	} \
}

SEXP bupidpopulate(SEXP R_file){
	struct iobtd iop;
	char *file = CHARPT(R_file,0);
	FILE *fd;
	SEXP scandf, peakdf, paramdf, moddf, protdf, searchdf, tagdf, fitdf, xlinkdf, xlpdf;
	SEXP tmp, ret, retnames;

	if(!(fd = fopen(file,"r")))
		return RNULL;

	io_init(&iop,fd);

	safedf(scandf, makedf_scan(&iop));
	safedf(peakdf, makedf_peak(&iop));
	safedf(paramdf, makedf_param(&iop));
	safedf(moddf, makedf_mod(&iop));
	safedf(protdf, makedf_prot(&iop));
	safedf(tagdf, makedf_tag(&iop));
	safedf(searchdf, makedf_search(&iop));
	safedf(fitdf, makedf_fit(&iop));
	safedf(xlinkdf, makedf_xlink(&iop));
	safedf(xlpdf, makedf_xlpep(&iop));

	make_list_names(retnames, 10, "scan", "decon", "param", "mod", "prot", "tag", "search", "fit", "xlink", "xlpep");
	make_list(ret, retnames, 10, scandf, peakdf, paramdf, moddf, protdf, tagdf, searchdf, fitdf, xlinkdf, xlpdf);

	io_close(&iop);

	unhideGC();

	return ret;
}
