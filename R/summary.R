#' Summarize BUPID results.
#' 
#' Print a summary for BUPID results
#' 
#' Each result from BTDR packages will provide differeny summaries
#' 
#' @param object
#' object returned from another function
#' @param type
#' the type of view to summarize
#' @param ...
#' extra parameters
#'
#' @return Returns the summary
#'
#' @examples
#' \dontrun{
#' server <- "http://bupid.bumc.bu.edu/cgi-bin/get_results.cgi"
#' infile <- "key=WBNqTswT5DPg3aDO&ID=320&date=20150309"
#' data <- read.bupid(url=paste(server,infile,sep="?"))
#' summary(data)
#' }
#' 
#' @name bupid-summary
NULL

#' @rdname bupid-summary
#' @export 
setMethod("summary",signature="bupid", definition=function(object,type="overview"){
	tmp <- getview(object,type)

	if(is.null(tmp))
		return(NULL)

	if(type == "fragment")
		.summary.overview(tmp)
	else if(type == "protein")
		.summary.protein(tmp)
	else
		.summary.overview(tmp)
})

.summary.overview <- function(object, ...){
	list(num.prot=nrow(object),num.scan=sum(object$scan.count),scan.counts=summary(object$scan.count))
}

.summary.protein <- function(object, ...){
	list(num.prot=nrow(object),protein.score=summary(object$protein.score),tag.coverage=summary(object$tag.coverage),tag.score=summary(object$tag.score))
}

.summary.fragment <- function(object, ...){
	length(object)
}
