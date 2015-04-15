#' Display BUPID results.
#' 
#' BUPID results
#' 
#' Each result from BTDR packages will provide differeny outputs
#' 
#' @param data
#' A bupid object
#' @param type
#' The type of output to display
#'
#' @return Returns the information
#'
#' @examples
#' server <- "http://bumc-florida.bumc.bu.edu/BUPID_TD/cgi-bin/get_results.cgi"
#' infile <- "key=WBNqTswT5DPg3aDO&ID=320&date=20150309"
#' data <- read.bupid(url=paste(server,infile,sep="?"))
#' print(data,"overview")
#' 
#' @name bupid-print
NULL

#' @rdname bupid-print
#' @export 
setMethod("print",signature="bupid", definition=function(x,type="overview"){
	getview(x,type)
})

#' @rdname bupid-print
#' @export 
setMethod("head",signature="bupid", definition=function(x,type="overview"){
	head(print(x,type))
})

#' @rdname bupid-print
#' @export 
setMethod("tail",signature="bupid", definition=function(x,type="overview"){
	tail(print(x,type))
})
