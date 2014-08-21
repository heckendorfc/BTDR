.geturldata <- function(url){
	curlopts <- list(useragent="BTDR",verbose=F,followLocation=T)
	url <- sub("results.html","cgi-bin/get_results.cgi",url)
	#stop("Reading from URL not yet supported")

	rawdata <- RCurl::getURL(url,followLocation=T,.opts=curlopts)

	if(rawdata=="")
		stop("This URL contains no information. Either the URL is invalid or the data is still processing.")

	yaml.load(rawdata)
}

read.bupid <- function(file=NULL,url=NULL){
	if(is.null(file) && is.null(url))
		stop("Either file or url must be provided.")

	if(is.null(file))
		res <- .geturldata(url)
	else
		res <- yaml.load_file(file)

	res
}
