setClass("internalbupid",representation(scan="data.frame",decon="data.frame",param="data.frame",prot="data.frame",search="data.frame",tag="data.frame",fit="data.frame"))

#' Class bupid
#'
#' BUPID Top-Down is a suite of tools designed to analyze top-down
#' proteomics data.
#'
#' A \code{bupid} object is an S4 class container that stores the results
#' of a BUPID Top-Down run and post-processing utilities.
#'
#' @slot internal
#' The internal structure used to populate views
#' @slot raw
#' Raw results from yaml.load used to populate internal
#' @slot view
#' The user-level structure containing a subset of the results
#' @slot type
#' The type of view currently active
#'
#' @keywords Class
#' @name bupid-class
setClass("bupid",representation(internal="internalbupid",raw="list",view="data.frame",type="character"))

setGeneric("getview",def=function(object,type){})
setMethod("getview",signature="bupid", definition=function(object,type){
	if(type=="overview"){
		pids <- sapply(as.character(unique(object@internal@prot$name)),FUN=function(x)which(object@internal@prot$name==x))
		#sids <- lapply(1:length(pids),FUN=function(x) object@internal@search$peakid %in% object@internal@prot$peakid[pids[[x]]])
		td <- data.frame(
				protein.name=names(pids),
				top.rank=sapply(1:length(pids),FUN=function(x)length(which(object@internal@prot$protid[pids[[x]]]==0))),
				scan.count=sapply(1:length(pids),FUN=function(x)length(pids[[x]])),
				row.names=NULL)

		#td <- data.frame(
				#protein.name=unique(object@internal@prot$name),
				#mean.rank=sapply(unique(object@internal@prot$name),FUN=function(x){
					#pid <- subset(object@internal@prot,name==x,select=protid)
					#sidb <- whichvec(object@internal@search$protid,pid)
					#mean(object@internal@search$rank[sidb])
				#}),
				#scan.count=sapply(unique(object@internal@prot$name),FUN=function(x)
					#nrow(unique(subset(object@internal@prot,name==x,select=peakid)))),
				#row.names=NULL)
		if(nrow(td)>0)
			td <- td[order(td$top.rank,decreasing=T),]
		td
	}
	else if(type=="protein"){
		td <- do.call("rbind",lapply(object@internal@prot$peakid,FUN=function(id){
			pdf <- subset(object@internal@prot,peakid==id)
			sdf <- subset(object@internal@search,peakid==id)
			data.frame(protein.name=pdf$name,
					   tag.coverage=sdf$cov,
					   tag.score=sdf$score,
					   tag.rank=sdf$rank,
					   row.names=id)
		}))
		td
	}
	else if(type=="fragment"){
		td <- do.call("rbind",lapply(unique(get_unique_prot_id(object@internal@fit$peak.id,object@internal@fit$protid)),FUN=function(id){
			fid <- which(get_unique_prot_id(object@internal@fit$peak.id,object@internal@fit$protid)==id)
			fdf <- object@internal@fit[fid,c("peak.index","peak.mass","peak.intensity","peak.z","ion.start","ion.len","ion.mass","frag","error")]
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
	}
})

get_unique_prot_id <- function(peakid,protid){
	paste(peakid,protid,sep="-")
}
get_unique_protid_list <- function(prot){
	get_unique_prot_id(prot$param$peaks$id,prot$id)
}

setGeneric("populate",def=function(object,data){})
setMethod("populate",signature="bupid", definition=function(object,data){
	scanres <- do.call("rbind",lapply(data$peaks,FUN=function(pl)data.frame(plid=rep(pl$id,length(pl$scans)),scanid=sapply(pl$scans,FUN=function(ps)ps$id),mz=sapply(pl$scans,FUN=function(ps)ps$mz),z=sapply(pl$scans,FUN=function(ps)ps$z))))
	deconres <- do.call("rbind",lapply(data$peaks,FUN=function(pl)data.frame(id=rep(pl$id,pl$num),mass=pl$mass,intensity=pl$intensity,z=pl$z)))
	paramres <- do.call("rbind",lapply(data$param,FUN=function(pl)data.frame(peakid=pl$peaks$id,frag=pl$frag,mstol=pl$mstol,msmstol=pl$msmstol,msmass=pl$msmass,tax=pl$tax)))
	protres <- do.call("rbind",lapply(data$prot,FUN=function(pl)data.frame(protid=pl$id,peakid=pl$param$peaks$id,seq=pl$seq,name=pl$name)))
	tagres <- do.call("rbind",lapply(data$search,FUN=function(sl)
		do.call("rbind",lapply(sl$tags,FUN=function(ttl,id)
			data.frame(searchid=id,start=ttl$start,length=ttl$len)
		,get_unique_protid_list(sl$prot)))
	))
	searchres <- do.call("rbind",lapply(data$search,FUN=function(sl)data.frame(searchid=get_unique_protid_list(sl$prot),peakid=sl$prot$param$peak$id,rank=sl$id,score=sl$score,cov=sl$cov)))
	fitres <- do.call("rbind",lapply(data$fit,FUN=function(fl){
		do.call("rbind",lapply(fl$results,FUN=function(fr,id,flp){
			data.frame(protid=id,
				peak.id=flp$id,
				peak.index=fr$peak+1,
				peak.mass=flp$mass[fr$peak+1],
				peak.intensity=flp$intensity[fr$peak+1],
				peak.z=flp$z[fr$peak+1],
				ion.start=fr$ion$start,
				ion.len=fr$ion$len,
				ion.mass=fr$ion$mass,
				frag=fr$frag,
				error=fr$err
				)}
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

	#object@raw <- data
	object@internal <- new("internalbupid",scan=scanres,decon=deconres,param=paramres,prot=protres,search=searchres,tag=tagres,fit=fitres)
	#object <- setview(object,"overview")

	object
})
