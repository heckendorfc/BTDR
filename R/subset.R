#' Subset BUPID results.
#' 
#' Return a subset of the BUPID results
#' 
#' Filter the results and return items matching the provided conditions.
#' 
#' @param x
#' object to be subsetted
#' @param subset
#' logical expression indicating elements or rows to keep. Valid elements are
#' found with names(head(x,select))
#' @param select
#' expression, indicating the return type
#' @param drop
#' drop unnecessary dimensions
#'
#' @return Returns the subset
#'
#' @examples
#' \dontrun{
#' server <- "http://bupid.bumc.bu.edu/cgi-bin/get_results.cgi"
#' infile <- "key=WBNqTswT5DPg3aDO&ID=320&date=20150309"
#' data <- read.bupid(url=paste(server,infile,sep="?"))
#' subset(data,name=="HBB","fragment")
#' }
#' 
#' @name bupid-subset
NULL

#' @rdname bupid-subset
#' @export 
setMethod("subset",signature="bupid", definition=function(x,subset,select,drop=FALSE){
	tmp <- getview(x,select) # returns a dataframe representation of the object
	rows <- eval(expr=substitute(subset), envir=tmp, enclos=sys.frame(which=-3))
	tmp <- tmp[rows,,drop=drop]

	if(select=="overview" || select=="protein"){
		l <- c("fit","search","tag","decon","scan","param","xlink","xlpep")

		if(select=="overview")
			pidsb <- whichvec(x@prot$name,tmp$protein.name)
		else if(select=="protein")
			pidsb <- whichvec(get_unique_prot_id(x@prot$peakid,x@prot$protid),row.names(tmp))

		x@prot <- x@prot[pidsb,]
		for(n in l)
			eval(parse(text=paste("x@",n,"<-",paste("filter",n,sep="."),"(x)",sep="")))
		return(x)
	} else if(select=="fragment"){
		l <- c("prot", "search","tag","decon","scan","param","xlink","xlpep")

		#fidsb <- rep(FALSE,nrow(x@fit))
		#fidsb[as.integer(row.names(tmp))] <- TRUE
		fidsb <- as.integer(row.names(tmp))

		x@fit <- x@fit[fidsb,]
		for(n in l)
			eval(parse(text=paste("x@",n,"<-",paste("filter",n,sep="."),"(x)",sep="")))
		return(x)
	} else if(grepl("raw-",select)){
		l <- c("fit", "prot", "search", "tag", "decon", "scan", "param",
			   "scan", "decon", "prot", "tag", "search", "fit","xlink","xlpep")
		type <- sub("raw-","",select)

		slot(x,type) <- tmp

		for(n in l)
			eval(parse(text=paste("x@",n,"<-",paste("filter",n,sep="."),"(x)",sep="")))
		return(x)
	}
})

whichvec <- function(tx,te){
	#sapply(tx,FUN=function(x)length(which(x==te))>0)
	tx %in% te
}

filter.param <- function(ib){
	idb <- whichvec(ib@param$id,ib@scan$parid)
	ib@param[idb,]
}

filter.scan <- function(ib){
	paramidb <- whichvec(ib@scan$parid,ib@param$id)
	scanidb <- whichvec(ib@scan$plid,ib@decon$id)
	ib@scan[scanidb&paramidb,]
}

filter.decon <- function(ib){
	scanidb <- whichvec(ib@decon$id,ib@scan$plid)
	protidb <- whichvec(ib@decon$id,ib@prot$peakid)
	ib@decon[protidb&scanidb,]
}

filter.prot <- function(ib){
	pid <- whichvec(ib@prot$peakid,ib@decon$id)
	ib@prot[pid,]
}

filter.tag <- function(ib){
	searchidb <- whichvec(ib@tag$searchid,ib@search$searchid)
	ib@tag[searchidb,]
}

filter.search <- function(ib){
	protidb <- whichvec(ib@search$searchid,get_unique_prot_id(ib@prot$peakid,ib@prot$protid))
	ib@search[protidb,]
}

filter.fit <- function(ib){
	protidb <- whichvec(get_unique_prot_id(ib@fit$peak.id,ib@fit$protid),get_unique_prot_id(ib@prot$peakid,ib@prot$protid))
	ib@fit[protidb,]
}

filter.xlink <- function(ib){
	protidb <- whichvec(ib@xlink$peakid,ib@fit$peak.id)
	ib@xlink[protidb,]
}

filter.xlpep <- function(ib){
	xlidb <- whichvec(ib@xlpep$xlid,ib@xlink$xlid)
	ib@xlpep[xlidb,]
}
