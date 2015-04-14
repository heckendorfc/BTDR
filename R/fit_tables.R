#' Peak fit tables
#'
#' These functions generate tables using information from the peak fit module.
#'
#' @param data
#' output from read.bupid
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

.get.frag.name <- function(res){
	term <- fragment.term(res$frag)

	name <- character(nrow(res))
	t1 <- which(term %in% c("N","C"))
	name[t1] <- res$ion.len[t1]
	t2 <- which(term=="")
	name[t2] <- paste(res$ion.start[t2]+1,res$ion.len[t2],sep=",")

	paste0(res$frag,"[",name,"]")
}

.ppm.error <- function(res){
	err <- res$error/res$peak.mass*1e6
	err <- round(err,digits=4)
	err
}

.matched.row <- function(res,peaks){
	name <- .get.frag.name(res)
	mod <- res$mods
	pkmass <- res$peak.mass
	pkint <- res$peak.intensity/max(res$peak.intensity)
	err <- .ppm.error(res)

	data.frame(name=name,mods=mod,start=res$ion.start+1,end=res$ion.start+res$ion.len,massE=pkmass,massT=res$ion.mass,intensity=pkint,ppmMassError=err,stringsAsFactors=F)
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
	.matched.row(data@internal@fit)
	#res <- do.call("rbind",lapply(data$fit[[fitid]]$results,.matched.row,data$peaks[[fitid]]))
	#colnames(res) <- c("name","mods","start","end","massE","massT","intensity","ppmMassError")

	#resdf <- .fix.df(as.data.frame(res))

	#res
}

.get.peak.name <- function(pid,data,peaks){
	paste(data$peak.mass[pid],paste0("(",(data$peak.intensity[pid]/max(data$peak.intensity))*100,"%)"),sep=" ")
}

.matched.peak.row <- function(pid,res,peaks){
	ri <- which(peaks==pid)
	ro <- order(res[ri,"error"])
	.get.frag.name(res[ri[ro],])
	#rlist <- which(lapply(res,FUN=function(ri)if(ri$peak==pid)T else F)==T)
	#ro <- order(sapply(rlist,FUN=function(x)res[[x]]$err))
	#sapply(ro,FUN=function(x).get.frag.name(res[[rlist[x]]]))
}

#' fit.matched.peaks generates the assigned fragments with matches grouped by matched peak.
#' 
#' @return Returns the list or matrix of assignments.
#' 
#' @rdname fit.matched
#' @export fit.matched.peaks
fit.matched.peaks <- function(data,format="list"){
	vd <- data@internal@fit
	peaks <- unique(get_unique_prot_id(vd$protid,vd$peak.index))
	#unique(sapply(data$fit[[fitid]]$results,FUN=function(res)res$peak))
	#peaks <- sort(peaks)
	res <- lapply(peaks,FUN=.matched.peak.row,vd,peaks)
	#peaks <- peaks+1
	#names(res) <- sapply(peaks,FUN=.get.peak.name,vd,peaks)
	names(res) <- paste(vd$peak.mass,paste0("(",(vd$peak.intensity/max(vd$peak.intensity))*100,"%)"),sep=" ")
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

.matched.cluster.row <- function(res){
	name <- .get.frag.name(res)
	pkmass <- res$peak.mass
	pkz <- res$peak.z
	pkint <- res$peak.intensity#[res$peak+1]/max(peaks$intensity)
	err <- .ppm.error(res)
	mz <- (pkmass+pkz*(1.007825035-0.000549))/pkz

	data.frame(name=name,intensity=pkint,ppmMassError=err,monoisotopicMZ=mz,z=pkz,stringsAsFactors=F)
}



#' fit.matched.clusters generates a matrix of assignments along with their
#' original m/z and charge state values as seen in the raw spectra.
#' 
#' @rdname fit.matched
#' @export fit.matched.clusters
fit.matched.clusters <- function(data,fitid=1L){
	vd <- getview(data,"fragment")
	.matched.cluster.row(vd)
}
