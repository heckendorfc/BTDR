#include <unistd.h>
#include <stdio.h>

#include "populate.h"

int io_general_init(yamldom_data_t *iod){
	memset(&iod->parser,0,sizeof(iod->event));
	memset(&iod->emitter,0,sizeof(iod->emitter));

	if(!yaml_emitter_initialize(&iod->emitter))
		goto error;

	if(iod->infd){
		if(!yaml_parser_initialize(&iod->parser))
			goto error;

		yaml_parser_set_input_file(&iod->parser, iod->infd);
		yaml_parser_set_encoding(&iod->parser,YAML_UTF8_ENCODING);
	}

	if(iod->outfd){
		yaml_emitter_set_output_file(&iod->emitter, iod->outfd);
		yaml_emitter_set_canonical(&iod->emitter,1);
		yaml_emitter_set_unicode(&iod->emitter,0);
	}

	return 0;

error:
	return 1;
}

int io_general_close(yamldom_data_t *iod){
	if(iod->infd)
		yaml_parser_delete(&iod->parser);

	if(iod->outfd)
		yaml_emitter_delete(&iod->emitter);

	return 0;
}

int find_anchor_id(yaml_document_t *doc, const char *name, int index){
	int m,r;
	yaml_node_t *par,*node;

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

	return 0;
}

SEXP safedf(SEXP df){
	if(df != RNULL)
		return df;

	return make_dataframe(RNULL,RNULL,0);
}

SEXP bupidpopulate(SEXP R_file){
	struct iobtd iop;
	char *file = CHARPT(R_file,0);
	FILE *fd;
	SEXP scandf, peakdf, paramdf, moddf, protdf, searchdf, tagdf, fitdf;
	SEXP ret, retnames;

	fd = fopen(file,"r");

	io_init(&iop,fd);

	scandf = safedf(makedf_scan(&iop));
	peakdf = safedf(makedf_peak(&iop));
	paramdf = safedf(makedf_param(&iop));
	moddf = safedf(makedf_mod(&iop));
	protdf = safedf(makedf_prot(&iop));
	tagdf = safedf(makedf_tag(&iop));
	searchdf = safedf(makedf_search(&iop));
	fitdf = safedf(makedf_fit(&iop));

	PROTECT(ret = allocVector(VECSXP,8));
	PROTECT(retnames = allocVector(STRSXP,8));

	SET_STRING_ELT(retnames, 0, mkChar("scan"));
	SET_STRING_ELT(retnames, 1, mkChar("decon"));
	SET_STRING_ELT(retnames, 2, mkChar("param"));
	SET_STRING_ELT(retnames, 3, mkChar("mod"));
	SET_STRING_ELT(retnames, 4, mkChar("prot"));
	SET_STRING_ELT(retnames, 5, mkChar("tag"));
	SET_STRING_ELT(retnames, 6, mkChar("search"));
	SET_STRING_ELT(retnames, 7, mkChar("fit"));

	SET_VECTOR_ELT(ret, 0, scandf);
	SET_VECTOR_ELT(ret, 1, peakdf);
	SET_VECTOR_ELT(ret, 2, paramdf);
	SET_VECTOR_ELT(ret, 3, moddf);
	SET_VECTOR_ELT(ret, 4, protdf);
	SET_VECTOR_ELT(ret, 5, tagdf);
	SET_VECTOR_ELT(ret, 6, searchdf);
	SET_VECTOR_ELT(ret, 7, fitdf);

	setAttrib(ret, R_NamesSymbol, retnames);

	io_close(&iop);

	UNPROTECT(2);

	return ret;
}
