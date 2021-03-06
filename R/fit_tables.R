#' Fragment assignment tables
#'
#' These functions generate tables using information from the fragment assignment module.
#'
#' @param data
#' output from read.bupid
#' @param format
#' decides the return type: list or matrix
#' 
#' @return Returns the assignment information table.
#'
#' @examples
#' \dontrun{
#' server <- "http://bupid.bumc.bu.edu/cgi-bin/get_results.cgi"
#' infile <- "key=WBNqTswT5DPg3aDO&ID=320&date=20150309"
#' data <- read.bupid(url=paste(server,infile,sep="?"))
#' fragment.matched.ions(data)
#' fragment.matched.peaks(data)
#' fragment.matched.clusters(data)
#' }
#' 
#' @name fragment.matched
NULL

.get.frag.name <- function(res){
	term <- fragment.term(res$frag)

	name <- character(nrow(res))
	t1 <- which(term %in% c("N","C"))
	name[t1] <- res$ion.len[t1]
	t2 <- which(term=="")
	name[t2] <- paste(res$ion.start[t2]+1,res$ion.len[t2],sep="...")

	paste0(res$frag,"[",name,"]")
}

.ppm.error <- function(res){
	err <- res$error/res$ion.mass*1e6
	err <- round(err,digits=4)
	err
}

.matched.row <- function(res,peaks){
	if(nrow(res)<1){
		return(NULL)
	}
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

#' fragment.matched.ions generates a matrix containing detailed information about the assigned fragments.
#' 
#' @return Returns the matrix of assignments.
#' 
#' @rdname fragment.matched
#' @export fragment.matched.ions
fragment.matched.ions <- function(data){
	.matched.row(data@fit)
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

#' fragment.matched.peaks generates the assigned fragments with matches grouped by matched peak.
#' 
#' @return Returns the list or matrix of assignments.
#' 
#' @rdname fragment.matched
#' @export fragment.matched.peaks
fragment.matched.peaks <- function(data,format="list"){
	vd <- data@fit
	uids <- get_unique_prot_id(vd$protid,vd$peak.index)
	peaks <- unique(uids)
	#unique(sapply(data$fit[[fitid]]$results,FUN=function(res)res$peak))
	#peaks <- sort(peaks)
	res <- lapply(peaks,FUN=.matched.peak.row,vd,uids)
	#peaks <- peaks+1
	#names(res) <- sapply(peaks,FUN=.get.peak.name,vd,peaks)
	vd <- vd[!duplicated(uids),]
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

.expand.fit.counts <- function(res){
	fit <- res@fit
	repinds <- which(fit$peak.count>1)
	if(length(repinds)>0){
		repfit <- do.call("rbind",lapply(repinds,FUN=function(ri){
			decind <- which(res@decon$id==fit$peak.id[ri])[fit$peak.index[ri]]
			do.call("rbind",lapply(2:fit$peak.count[ri],FUN=function(rci){
				row <- fit[ri,]
				row$peak.mass <- res@decon$mass[decind+rci]
				row$peak.intensity <- res@decon$intensity[decind+rci]
				row$peak.z <- res@decon$z[decind+rci]
				row$peak.index <- decind+rci
				row
			}))
		}))
		repfit$peak.count <- 1;
		fit <- rbind(fit,repfit)
	}
	fit
}

.mass.to.mz <- function(mass,z){
	(mass+z*(1.007825035-0.000549))/z
}

.matched.cluster.row <- function(res){
	tfit <- .expand.fit.counts(res)

	scans <- sapply(tfit$peak.id,FUN=function(x){
		paste(res@scan$scanid[which(res@scan$plid==x)],collapse=" ")
	})
	name <- .get.frag.name(tfit)
	pkmass <- tfit$peak.mass
	pkz <- tfit$peak.z
	pkint <- tfit$peak.intensity#[res$peak+1]/max(peaks$intensity)
	err <- .ppm.error(tfit)
	mz <- .mass.to.mz(pkmass,pkz)


	data.frame(scanIDs=scans,name=name,intensity=pkint,ppmMassError=err,monoisotopicMZ=mz,z=pkz,stringsAsFactors=F)
}



#' fragment.matched.clusters generates a matrix of assignments along with their
#' original m/z and charge state values as seen in the raw spectra.
#' 
#' @rdname fragment.matched
#' @export fragment.matched.clusters
fragment.matched.clusters <- function(data){
	#vd <- getview(data,"fragment")
	.matched.cluster.row(data)
}
