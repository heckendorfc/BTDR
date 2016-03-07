#' Summarize BUPID results.
#' 
#' Print a summary for BUPID results
#' 
#' Each result from BTDR packages will provide differeny summaries
#' 
#' @param data
#' data returned from another function
#' @param ...
#' extra parameters
#'
#' @return Returns the summary
#'
#' @examples
#' server <- "http://bupid.bumc.bu.edu/BUPID_TD/cgi-bin/get_results.cgi"
#' infile <- "key=WBNqTswT5DPg3aDO&ID=320&date=20150309"
#' data <- read.bupid(url=paste(server,infile,sep="?"))
#' summary(data)
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
summary.overview <- function(data, ...){
	class(data) <- "data.frame"
	list(num.prot=nrow(data),num.scan=sum(data$scan.count),scan.counts=summary(data$scan.count))
}

#' @rdname bupid-summary
#' @export 
summary.protein <- function(data, ...){
	class(data) <- "data.frame"
	list(num.prot=nrow(data),protein.score=summary(data$protein.score),tag.coverage=summary(data$tag.coverage),tag.score=summary(data$tag.score))
}

#' @rdname bupid-summary
#' @export 
summary.fragment <- function(data, ...){
	length(data)
}

