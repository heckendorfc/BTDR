#' Peak fit tables
#'
#' These functions generate tables using information from the peak fit module.
#'
#' @param data
#' output from read.bupid
#' @param fitid
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

.modstr <- function(mods){
	if(length(mods)<1)
		""
	else{
		str <- sapply(mods,FUN=function(mod) paste(mod$mod$name,mod$num,sep="x"))
		paste(str,collapse=" ")
	}
}

.get.frag.name <- function(res){
	term <- fragment.term(res$frag)
	if(!is.null(term) && (term=="N" || term=="C"))
		name <- res$ion$len
	else
		name <- paste(res$ion$start+1,res$ion$len,sep=",")

	paste0(res$frag,"[",name,"]")
}

.matched.row <- function(res,peaks){
	name <- .get.frag.name(res)
	mod <- .modstr(res$mods)
	pkmass <- peaks$mass[res$peak+1]
	pkint <- peaks$intensity[res$peak+1]/max(peaks$intensity)
	err <- res$err/pkmass*1e6
	err <- round(err,digits=4)

	data.frame(name=name,mods=mod,start=res$ion$start+1,end=res$ion$start+res$ion$len,massE=pkmass,massT=res$ion$mass,intensity=pkint,ppmMassError=err,stringsAsFactors=F)
}

.fix.df <- function(df){
	cbind(df[!sapply(df, is.list)],(t(apply(df[sapply(df, is.list)], 1, unlist))))
}



#' fit.matched.ions generates a matrix containing detailed information about the assigned fragments.
#' 
#' @return Returns the matrix of assignments.
#' 
#' @rdname fit.matched
#' @export fit.matched.ions
fit.matched.ions <- function(data,fitid=1L){
	res <- do.call("rbind",lapply(data$fit[[fitid]]$results,.matched.row,data$peaks[[fitid]]))
	#colnames(res) <- c("name","mods","start","end","massE","massT","intensity","ppmMassError")

	#resdf <- .fix.df(as.data.frame(res))

	res
}

.get.peak.name <- function(pid,peaks){
	paste(peaks$mass[pid],paste0("(",(peaks$intensity[pid]/max(peaks$intensity))*100,"%)"),sep=" ")
}

.matched.peak.row <- function(pid,res,peaks){
	rlist <- which(lapply(res,FUN=function(ri)if(ri$peak==pid)T else F)==T)
	ro <- order(sapply(rlist,FUN=function(x)res[[x]]$err))
	sapply(ro,FUN=function(x).get.frag.name(res[[rlist[x]]]))
}

#' fit.matched.peaks generates the assigned fragments with matches grouped by matched peak.
#' 
#' @return Returns the list or matrix of assignments.
#' 
#' @rdname fit.matched
#' @export fit.matched.peaks
fit.matched.peaks <- function(data,fitid=1L,format="list"){
	peaks <- unique(sapply(data$fit[[fitid]]$results,FUN=function(res)res$peak))
	peaks <- sort(peaks)
	res <- lapply(peaks,FUN=.matched.peak.row,data$fit[[fitid]]$results,data$peaks[[fitid]])
	peaks <- peaks+1
	names(res) <- sapply(peaks,FUN=.get.peak.name,data$peaks[[fitid]])
	if(format=="list")
		res
	else{ #matrix
		rows <- length(res)
		cols <- max(sapply(res,length))
		mres <- matrix(nrow=rows,ncol=cols)
		for(i in 1:rows)
			mres[i,] <- sapply(1:cols,FUN=function(ci)if(ci<=length(res[[i]])) res[[i]][ci] else NA)
		row.names(mres) <- names(res)
		mres
	}
}

.matched.cluster.row <- function(res,peaks){
	name <- .get.frag.name(res)
	pkmass <- peaks$mass[res$peak+1]
	pkz <- peaks$z[res$peak+1]
	pkint <- peaks$intensity[res$peak+1]/max(peaks$intensity)
	err <- res$err/pkmass*1e6
	err <- round(err,digits=4)
	mz <- (pkmass+pkz*(1.007825035-0.000549))/pkz

	data.frame(name=name,intensity=pkint,ppmMassError=err,monoisotopicMZ=mz,z=pkz,stringsAsFactors=F)
}



#' fit.matched.clusters generates a matrix of assignments along with their
#' original m/z and charge state values as seen in the raw spectra.
#' 
#' @rdname fit.matched
#' @export fit.matched.clusters
fit.matched.clusters <- function(data,fitid=1L){
	res <- do.call("rbind",lapply(data$fit[[fitid]]$results,.matched.cluster.row,data$peaks[[fitid]]))

	res
}
