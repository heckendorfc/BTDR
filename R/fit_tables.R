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

matched.ions <- function(data,peaklistid=1L){
	res <- do.call("rbind",lapply(data$fit[[peaklistid]]$results,.matched.row,data$peaks[[peaklistid]]))
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

matched.peaks <- function(data,peaklistid=1L,format="list"){
	peaks <- unique(sapply(data$fit[[peaklistid]]$results,FUN=function(res)res$peak))
	peaks <- sort(peaks)
	res <- lapply(peaks,FUN=.matched.peak.row,data$fit[[peaklistid]]$results,data$peaks[[peaklistid]])
	peaks <- peaks+1
	names(res) <- sapply(peaks,FUN=.get.peak.name,data$peaks[[peaklistid]])
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

matched.clusters <- function(data,peaklistid=1L){
	res <- do.call("rbind",lapply(data$fit[[peaklistid]]$results,.matched.cluster.row,data$peaks[[peaklistid]]))

	res
}
