#' Summarize BUPID results.
#' 
#' Print a summary for BUPID results
#' 
#' Each result from BTDR packages will provide differeny summaries
#' 
#' @param object
#' object returned from another function
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

#setGeneric("summary",def=function(object){})

#' @rdname bupid-summary
#' @export 
setMethod("summary",signature="bupid", definition=function(object,type="overview"){
	#tmp <- object@view
	tmp <- getview(object,type)

	if(is.null(tmp))
		return(NULL)

	class(tmp) <- type
	#class(tmp) <- object@type
	summary(tmp)
})

#' @rdname bupid-summary
#' @export 
summary.overview <- function(object, ...){
	class(object) <- "object.frame"
	list(num.prot=nrow(object),num.scan=sum(object$scan.count),scan.counts=summary(object$scan.count))
}

#' @rdname bupid-summary
#' @export 
summary.protein <- function(object, ...){
	class(object) <- "object.frame"
	list(num.prot=nrow(object),protein.score=summary(object$protein.score),tag.coverage=summary(object$tag.coverage),tag.score=summary(object$tag.score))
}

#' @rdname bupid-summary
#' @export 
summary.fragment <- function(object, ...){
	length(object)
}
