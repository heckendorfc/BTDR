.geturldata <- function(url){
	curlopts <- list(useragent="BTDR",verbose=F,followLocation=T)
	url <- sub("results.html","cgi-bin/get_results.cgi",url)
	#stop("Reading from URL not yet supported")

	rawdata <- RCurl::getURL(url,followLocation=T,.opts=curlopts)

	if(rawdata=="")
		stop("This URL contains no information. Either the URL is invalid or the data is still processing.")

	yaml.load(rawdata)
}

#' Read BUPID results.
#' 
#' Read BUPID results and create and object from them.
#' 
#' The data can be passed either as the name of a file on the local machine or
#' a URL on the BUPID server containing the results.
#' 
#' @param file
#' a file on the local machine to use as input
#' @param url
#' a url where the data can be retreived
#'
#' @return Returns the results as a bupid object.
#'
#' @examples
#' server <- "http://bupid.bumc.bu.edu/BUPID_TD/cgi-bin/get_results.cgi"
#' infile <- "key=WBNqTswT5DPg3aDO&ID=320&date=20150309"
#' data <- read.bupid(url=paste(server,infile,sep="?"))
#' 
#' @export read.bupid
read.bupid <- function(file=NULL,url=NULL){
	if(is.null(file) && is.null(url))
		stop("Either file or url must be provided.")

	if(is.null(file))
		res <- .geturldata(url)
	else
		res <- yaml.load_file(file)

	cres <- bupidpopulate(res)

	cres
}
