.pf.info <- function(fid,data){
	name <- data$fit[[fid]]$prot$name
	mi <- fit.matched.ions(data,fid)
	nfrag <- nrow(mi)
	scan <- data$fit[[fid]]$prot$param$peaks$scans[[1]]$id

	data.frame(ScanNum=scan,ProteinName=name,NumFrag=nfrag)
}

#' Display results from LC-MS/MS data
#' 
#' Display results from LC-MS/MS data processed with BUPID Top-Down
#' 
#' @param data
#' output from read.bupid
#' 
#' @return Returns a matrix with the results
#' @examples
#' server <- "http://bumc-florida.bumc.bu.edu/BUPID_TD/cgi-bin/get_results.cgi"
#' #TODO: use a real LC-MS/MS search result :)
#' infile <- "key=WBNqTswT5DPg3aDO&ID=320&date=20150309"
#' data <- read.bupid(url=paste(server,infile,sep="?"))
#' lcms.proteins(data)
#' 
#' @export lcms.proteins
lcms.proteins <- function(data){
	res <- do.call("rbind",lapply(1:length(data$fit),.pf.info,data))
	res
}
