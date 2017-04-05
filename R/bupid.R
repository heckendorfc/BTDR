#setClass("internalbupid",representation(scan="data.frame",decon="data.frame",param="data.frame",prot="data.frame",search="data.frame",tag="data.frame",fit="data.frame"))

#' Class bupid
#'
#' BUPID Top-Down is a suite of tools designed to analyze top-down
#' proteomics data.
#'
#' A \code{bupid} object is an S4 class container that stores the results
#' of a BUPID Top-Down run and post-processing utilities.
#'
#' @slot scan
#' Scan header information
#' @slot decon
#' Peak lists
#' @slot param
#' Parameters used for each peak list
#' @slot prot
#' Protein candidates selected
#' @slot search
#' Database search results
#' @slot tag
#' Sequence tags identified
#' @slot fit
#' Fragment ions identified
#' @slot xlink
#' Peak lists with crosslinked ions identified
#' @slot xlpep
#' Crosslinked ions identified
#'
#' @keywords Class
#' @name bupid-class
setClass("bupid",representation(scan="data.frame",decon="data.frame",param="data.frame",mod="data.frame",prot="data.frame",search="data.frame",tag="data.frame",fit="data.frame",xlink="data.frame",xlpep="data.frame"))

setGeneric("getview",def=function(object,type){})
setMethod("getview",signature="bupid", definition=function(object,type){
	if(type=="overview"){
		pnames <- as.character(unique(object@prot$name))
		pids <- lapply(pnames,FUN=function(x)which(object@prot$name==x))
		#sids <- lapply(1:length(pids),FUN=function(x) object@search$peakid %in% object@prot$peakid[pids[[x]]])
		td <- data.frame(
				protein.name=pnames,
				top.rank=sapply(1:length(pids),FUN=function(x)length(which(object@prot$protid[pids[[x]]]==0))),
				scan.count=sapply(1:length(pids),FUN=function(x)length(pids[[x]])),
				row.names=NULL)

		#td <- data.frame(
				#protein.name=unique(object@prot$name),
				#mean.rank=sapply(unique(object@prot$name),FUN=function(x){
					#pid <- subset(object@prot,name==x,select=protid)
					#sidb <- whichvec(object@search$protid,pid)
					#mean(object@search$rank[sidb])
				#}),
				#scan.count=sapply(unique(object@prot$name),FUN=function(x)
					#nrow(unique(subset(object@prot,name==x,select=peakid)))),
				#row.names=NULL)
		if(nrow(td)>0)
			td <- td[order(td$top.rank,decreasing=T),]
		td
	} else if(type=="protein"){
		td <- do.call("rbind",lapply(unique(object@prot$peakid),FUN=function(id){
			pdf <- subset(object@prot,peakid==id)
			sdf <- subset(object@search,peakid==id)
			data.frame(protein.name=pdf$name,
					   protein.start=pdf$start,
					   protein.len=pdf$len,
					   protein.score=sdf$score,
					   tag.coverage=sdf$cov,
					   tag.score=sdf$tagscore,
					   tag.rank=sdf$rank,
					   scan.num=paste(object@scan$scanid[which(object@scan$plid==id)],collapse=" "),
					   row.names=get_unique_prot_id(id,sdf$rank))
		}))
		td
	} else if(type=="fragment"){
		td <- do.call("rbind",lapply(unique(get_unique_prot_id(object@fit$peak.id,object@fit$protid)),FUN=function(id){
			fid <- which(get_unique_prot_id(object@fit$peak.id,object@fit$protid)==id)
			fdf <- object@fit[fid,c("peak.index","peak.mass","peak.intensity","peak.z","ion.start","ion.len","ion.mass","frag","mods","error")]
			row.names(fdf) <- fid
			#data.frame(peak.index=fdf$peak.index,
					   #peak.mass=fdf$peak.mass,
					   #peak.intensity=fdf$peak.intensity,
					   #peak.z=fdf$peak.z,
					   #ion.start=fdf$ion.start,
					   #ion.len=fdf$ion.len,
					   #ion.mass=fdf$ion.mass,
					   #frag=fdf$frag,
					   #error=fdf$error,
					   #row.names=fid)
			fdf
		}))
		#row.names(td) <- NULL
		td
	} else if(grepl("raw-",type)){
		type <- sub("raw-","",type)
		slot(object,type)
	}
})

get_unique_prot_id <- function(peakid,protid){
	paste(peakid,protid,sep="-")
}
get_unique_protid_list <- function(prot){
	get_unique_prot_id(prot$param$peaks$id,prot$id)
}

.modstr <- function(mods){
	if(length(mods)<1)
		""
	else{
		str <- sapply(mods,FUN=function(mod) paste(mod$mod$name,mod$num,sep="x"))
		paste(str,collapse=" ")
	}
}

.get.mod.field <- function(param,field){
	c(unlist(sapply(param[["fmod"]],FUN=function(fm)fm[field])),
	  unlist(sapply(param[["vmod"]],FUN=function(vm)vm[field])))
}

bupidpopulate <- function(data){
	scanres <- do.call("rbind",lapply(data$peaks,FUN=function(pl)data.frame(plid=rep(pl$id,length(pl$scans)),scanid=sapply(pl$scans,FUN=function(ps)ps$id),mz=sapply(pl$scans,FUN=function(ps)ps$mz),z=sapply(pl$scans,FUN=function(ps)ps$z),rt=sapply(pl$scans,FUN=function(ps)if(length(ps$rt)>0)ps$rt else 0))))
	deconres <- do.call("rbind",lapply(data$peaks,FUN=function(pl)data.frame(id=rep(pl$id,pl$num),mass=pl$mass,intensity=pl$intensity,z=pl$z)))
	paramres <- do.call("rbind",lapply(data$param,FUN=function(pl)data.frame(peakid=pl$peaks$id,frag=pl$frag,mstol=pl$mstol,msmstol=pl$msmstol,msmass=pl$msmass,tax=pl$tax)))
	modres <- do.call("rbind",lapply(data$param,FUN=function(pl){
										fixed <- c(rep(1,length(pl$fmod)),rep(0,length(pl$vmod)))
										#print(class(.get.mod.field(pl,"mass")))
										#print(pl["vmod"][[1]])
										if(length(fixed)==0){
											NULL
										} else{
											data.frame(paramid=rep(pl$peaks$id,length(fixed)),
													   fixed=fixed,
													   name=.get.mod.field(pl,"name"),
													   mass=.get.mod.field(pl,"mass"),
													   pos=.get.mod.field(pl,"pos"),
													   site=.get.mod.field(pl,"site"))
										}}))
	protres <- do.call("rbind",lapply(data$prot,FUN=function(pl){
		ret <- data.frame(protid=pl$id,peakid=pl$param$peaks$id,seq=pl$seq,name=pl$name)
		if(length(pl$start)==0){ # legacy support :(
			if(grepl("\\[[0-9-]*\\]$",pl$name)){
				nn <- as.integer(sub(".*\\[([0-9]*)-[0-9]*\\]$","\\1",pl$name))
				start <- nn[1]-1
				ret$name <- sub("\\[[0-9-]*\\]$","",pl$name)
			} else {
				start <- 0
			}
			len <- nchar(pl$seq)
		} else {
			start <- pl$start
			len <- pl$len
		}
		ret$start <- start
		ret$len <- len
		ret
	}))
	tagres <- do.call("rbind",lapply(data$search,FUN=function(sl)
		do.call("rbind",lapply(sl$tags,FUN=function(ttl,id)
			data.frame(searchid=id,start=ttl$start,length=ttl$len)
		,get_unique_protid_list(sl$prot)))
	))
	searchres <- do.call("rbind",lapply(data$search,FUN=function(sl)data.frame(searchid=get_unique_protid_list(sl$prot),peakid=sl$prot$param$peak$id,rank=sl$id,score=sl$score,tagscore=sl$tagscore,cov=sl$cov)))
	fitres <- do.call("rbind",lapply(data$fit,FUN=function(fl){
		#rdf <- fl$results
		#peakid <- rdf$peak+1
		#flp <- fl$prot$param$peaks
		#n <- length(peakid)
		#df <- data.frame(protid=rep(fl$prot$id,n),
						 #peak.id=rep(flp$id,n),
						 #peak.index=peakid,
						 #peak.mass=flp$mass[peakid],
						 #peak.intensity=flp$intensity[peakid],
						 #peak.z=flp$z[peakid],
						 #ion.start=rdf$ion.start,
						 #ion.len=rdf$ion.len,
						 #ion.mass=rdf$ion.mass,
						 #frag=rdf$frag,
						 #mods=rdf$mods,
						 #error=rdf$err
				   #)
		#return(df)

		do.call("rbind",lapply(fl$results,FUN=function(fr,id,flp){
			ret <- data.frame(protid=id,
				peak.id=flp$id,
				peak.index=fr$peak+1,
				peak.count=fr$peakcount,
				ion.start=fr$ion$start,
				ion.len=fr$ion$len,
				ion.mass=flp$mass[fr$peak+1]-fr$err,
				frag=fr$frag,
				mods=.modstr(fr$mods),
				error=fr$err
				)
			ret$peak.count[which(ret$peak.count<1)] <- 1 # support legacy files
			inds <- (ret$peak.index):(ret$peak.index+ret$peak.count-1)
			maxind <- inds[order(flp$intensity[inds],decreasing=T)[1]]
			ret$peak.intensity <- flp$intensity[maxind]
			ret$peak.mass <- flp$mass[maxind]
			ret$peak.z <- flp$z[maxind]
			ret
			}
		,fl$prot$id,fl$prot$param$peaks))
	}))

	if(is.null(paramres))
		paramres <- data.frame()
	if(is.null(protres))
		protres <- data.frame()
	if(is.null(tagres))
		tagres <- data.frame()
	if(is.null(searchres))
		searchres <- data.frame()
	if(is.null(fitres))
		fitres <- data.frame()
	if(is.null(modres))
		modres <- data.frame()

	object <- new("bupid",scan=scanres,decon=deconres,param=paramres,mod=modres,prot=protres,search=searchres,tag=tagres,fit=fitres,xlink=data.frame(),xlpep=data.frame())

	object
}
