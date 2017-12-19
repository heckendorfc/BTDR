.peprow <- function(n,x,prot){
	pprot <- prot[which(prot$protid==x$protid[n]),]
	pr <- data.frame(pep.protid=pprot$protid,
					 pep.start=pprot$start+1,
					 pep.end=pprot$start+pprot$len,
					 pep.mass=x$pep.mass[n],
					 pep.xlsite=x$pep.xlsite[n],
					 pep.seq=pprot$seq)
	names(pr) <- sub("pep",paste0("pep",n),names(pr))
	pr
}

#' Crosslink peptide subset
#'
#' Subset the bupid object by peptide number
#' 
#' @param object
#' bupid object returned from another function
#' @param pepnum
#' peptide number
#'
#' @return Returns a bupid object with results for one peptide
#' 
#' @examples
#' \dontrun{
#' server <- "http://bupid.bumc.bu.edu/cgi-bin/get_results.cgi"
#' infile <- "key=WBNqTswT5DPg3aDO&ID=320&date=20150309"
#' data <- read.bupid(url=paste(server,infile,sep="?"))
#' sdata <- subset(data,scan.num=234,"protein")
#' fragment.matched.ions(xlinkpep(sdata,1))
#' }
#' @export xlinkpep
xlinkpep <- function(object,pepnum){
	prot <- pepnum-1
	subset(object,tag.rank==prot,"protein")
}

.xlfragcov <- function(object,xldf){
	pid <- object@scan$plid[which(object@scan$scanid==xldf$scan.num)]
	lens <- (xldf$pep1.end-xldf$pep1.start+1)+(xldf$pep2.end-xldf$pep2.start+1)
	fr=object@fit[which(object@fit$peak.id==pid),c("ion.start","ion.len")]
	nrow(unique(fr))/(lens*2-2)
}


#' Crosslink results table
#'
#' Create a dataframe of xlink results
#' 
#' @param object
#' bupid object returned from another function
#' @param n
#' number of linked peptides to display per scan
#'
#' @return Returns a data.frame with the assignments.
#' 
#' @examples
#' \dontrun{
#' server <- "http://bupid.bumc.bu.edu/cgi-bin/get_results.cgi"
#' infile <- "key=WBNqTswT5DPg3aDO&ID=320&date=20150309"
#' data <- read.bupid(url=paste(server,infile,sep="?"))
#' xlinks(data,2)
#' }
#' @export xlinks
xlinks <- function(object,n=2){
	td <- do.call("rbind",lapply(unique(object@xlink$peakid),FUN=function(id){
		xid <- which(object@xlink$peakid==id)
		xpid <- which(object@xlpep$xlid==object@xlink$xlid[xid[1]])
		if(length(xpid)!=n)
			return(data.frame())

		scans <- object@scan[which(object@scan$plid == id),]
		pre <- data.frame(scan.num=paste(scans$scanid,collapse=" "),
							pre.int=scans$pre.int[1],
							pre.mz=scans$mz[1],
							pre.z=scans$z[1],
							pre.error=object@xlink$error[xid[1]],
							frag.cov=0, # fill later
							mods=object@xlink$mods[xid[1]])
		xlprot <- subset(object@prot,peakid==id)
		peps <- do.call("cbind",lapply(1:length(xpid),FUN=.peprow,object@xlpep[xpid,],xlprot))
		xldf <- cbind(pre,peps)
		xldf$frag.cov <- .xlfragcov(object,xldf) # TODO: vectorize
		xldf
	}))
	td
}
