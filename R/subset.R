#' Subset BUPID results.
#' 
#' Return a subset of the BUPID results
#' 
#' Filter the results and return items matching the provided conditions.
#' 
#' @param x
#' object to be subsetted
#' @param subset
#' logical expression indicating elements or rows to keep
#' @param select
#' expression, indicating the return type
#'
#' @return Returns the subset
#'
#' @examples
#' server <- "http://bumc-florida.bumc.bu.edu/BUPID_TD/cgi-bin/get_results.cgi"
#' infile <- "key=WBNqTswT5DPg3aDO&ID=320&date=20150309"
#' data <- read.bupid(url=paste(server,infile,sep="?"))
#' subset(data,name=="HBB","fragment")
#' 
#' @name bupid-subset
NULL

#' @rdname bupid-subset
#' @export 
setMethod("subset",signature="bupid", definition=function(x,subset,select,drop=FALSE){
	tmp <- getview(x,select) # returns a dataframe representation of the object
	rows <- eval(expr=substitute(subset), envir=tmp, enclos=parent.frame())
	tmp <- tmp[rows,,drop=drop]

	if(select=="overview" || select=="protein"){
		l <- c("fit","search","tag","param","decon","scan")

		if(select=="overview")
			pidsb <- whichvec(x@internal@prot$name,tmp$protein.name)
		else if(select=="protein")
			pidsb <- whichvec(x@internal@prot$name,row.names(tmp))

		x@internal@prot <- x@internal@prot[pidsb,]
		for(n in l)
			eval(parse(text=paste("x@internal@",n,"<-",paste("filter",n,sep="."),"(x@internal)",sep="")))
		return(x)
	}
	else if(select=="fragment"){
		l <- c("prot", "search","tag","param","decon","scan")

		#fidsb <- rep(FALSE,nrow(x@internal@fit))
		#fidsb[as.integer(row.names(tmp))] <- TRUE
		fidsb <- as.integer(row.names(tmp))

		x@internal@fit <- x@internal@fit[fidsb,]
		for(n in l)
			eval(parse(text=paste("x@internal@",n,"<-",paste("filter",n,sep="."),"(x@internal)",sep="")))
		return(x)
	}
})

whichvec <- function(tx,te){
	#sapply(tx,FUN=function(x)length(which(x==te))>0)
	tx %in% te
}

filter.scan <- function(ib){
	scanidb <- whichvec(ib@scan$plid,ib@decon$id)
	ib@scan[scanidb,]
}

filter.decon <- function(ib){
	paramidb <- whichvec(ib@decon$id,ib@param$peakid)
	scanidb <- whichvec(ib@decon$id,ib@scan$plid)
	ib@decon[paramidb&scanidb,]
}

filter.param <- function(ib){
	peakidb <- whichvec(ib@param$peakid,ib@decon$id)
	protidb <- whichvec(ib@param$peakid,ib@prot$peakid)
	ib@param[peakidb&protidb,]
}

filter.tag <- function(ib){
	searchidb <- whichvec(ib@tag$searchid,ib@search$searchid)
	ib@tag[searchidb,]
}

filter.search <- function(ib){
	#peakidb <- whichvec(ib@search$peakid,ib@prot$peakid)
	#rankidb <- whichvec(ib@search$rank,ib@prot$protid)
	protidb <- whichvec(ib@search$searchid,get_unique_prot_id(ib@prot$peakid,ib@prot$protid))
	#ib@search[rankidb&peakidb,]
	ib@search[protidb,]
}

filter.fit <- function(ib){
	#protidb <- whichvec(ib@fit$peak.id,ib@prot$peakid)
	protidb <- whichvec(get_unique_prot_id(ib@fit$peak.id,ib@fit$protid),get_unique_prot_id(ib@prot$peakid,ib@prot$protid))
	ib@fit[protidb,]
}

filter.prot <- function(ib){
	# is this only called after fit filter?
	#sid <- whichvec(get_unique_protid(ib@prot$peakid,ib@prot$protid),ib@search$searchid)
	fid <- whichvec(get_unique_prot_id(ib@prot$peakid,ib@prot$protid),get_unique_prot_id(ib@fit$peak.id,ib@fit$protid))
	ib@prot[fid,]
}