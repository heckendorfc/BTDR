#' Display BUPID results.
#' 
#' BUPID results
#' 
#' Each result from BTDR packages will provide different outputs
#' 
#' @param x
#' A bupid object
#' @param type
#' The type of output to display. One of the strings returned by
#' c("overview","protein","fragment") or
#' paste("raw",slotNames(data),sep="-")
#' @param n
#' The number of items to display
#'
#' @return Returns the information
#'
#' @examples
#' \dontrun{
#' server <- "http://bupid.bumc.bu.edu/cgi-bin/get_results.cgi"
#' infile <- "key=WBNqTswT5DPg3aDO&ID=320&date=20150309"
#' data <- read.bupid(url=paste(server,infile,sep="?"))
#' print(data,"overview")
#' head(data,"overview",10)
#' tail(data,"overview",2)
#' }
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
setMethod("head",signature="bupid", definition=function(x,type="overview",n=6){
	utils::head(print(x,type),n)
})

#' @rdname bupid-print
#' @export 
setMethod("tail",signature="bupid", definition=function(x,type="overview",n=6){
	utils::tail(print(x,type),n)
})
