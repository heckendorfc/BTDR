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
setMethod("print",signature="bupid", definition=function(x,type){
	if(type=="overview"){
		td <- data.frame(protein.name=unique(x@internal@prot$name),scan.count=sapply(unique(x@internal@prot$name),FUN=function(x)length(which(x@internal@prot$name==x))),row.names=NULL)
		td <- td[order(td$scan.count,decreasing=T),]
		td
	}
})
