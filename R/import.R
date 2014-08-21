.getfile <- function(url){
	url <- sub("results.html","cgi-bin/get_results.cgi",url)
	stop("Reading from URL not yet supported")
}

read.bupid <- function(file=NULL,url=NULL){
	if(is.null(file) && is.null(url))
		stop("Either file or url must be given.")

	if(is.null(file))
		file <- .getfile(url)

	res <- yaml.load_file(file)

	res
}
