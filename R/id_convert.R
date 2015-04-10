#' Convert BUPID Module IDs
#'
#' These functions translate IDs between different modules.
#'
#' @param data
#' output from read.bupid
#' @param id
#' peak fit result index to process
#' @param format
#' decides the return type: list or matrix
#' 
#' @return Returns the assignment information table.
#'
#' @examples
#' server <- "http://bumc-florida.bumc.bu.edu/BUPID_TD/cgi-bin/get_results.cgi"
#' infile <- "key=WBNqTswT5DPg3aDO&ID=320&date=20150309"
#' data <- read.bupid(url=paste(server,infile,sep="?"))
#' fit.matched.ions(data)
#' fit.matched.peaks(data)
#' fit.matched.clusters(data)
#' 
#' @name fit.matched
NULL

.fitid.by.peaklistid <- function(data,peaklistid){
	fi <- which(sapply(1:length(data$fit),FUN=function(df)if(data$fit[[df]]$prot$param$peaks$id==peaklistid)df else 0)>0)
	fi
}


