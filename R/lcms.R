.pf.info <- function(plid,data){
	name <- data$fit[[plid]]$prot$name
	mi <- matched.ions(data,plid)
	nfrag <- nrow(mi)
	scan <- data$fit[[plid]]$prot$param$peaks$scans[[1]]$id

	data.frame(ScanNum=scan,ProteinName=name,NumFrag=nfrag)
}

lcms.proteins <- function(data){
	res <- do.call("rbind",lapply(1:length(data$fit),.pf.info,data))
	res
}
