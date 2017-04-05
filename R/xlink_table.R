.peprow <- function(n,x,prot){
	seq <- prot$seq[which(prot$protid==x$protid)]
	pr <- x[n,c("pep.start","pep.len","pep.mass")]
	pr$pep.len <- pr$pep.start+pr$pep.len
	pr$pep.start <- pr$pep.start+1
	pr$pep.seq <- substr(seq[1],pr$pep.start,pr$pep.len)
	names(pr) <- paste(sub("len","end",names(pr)),n,sep="")
	pr
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
#' xlink.table(data,2)
#' }
#' @export xlinks
xlinks <- function(object,n=2){
	td <- do.call("rbind",lapply(unique(object@xlink$peakid),FUN=function(id){
		xid <- which(object@xlink$peakid==id)
		xpid <- which(object@xlpep$xlid==object@xlink$xlid[xid[1]])
		if(length(xpid)!=n)
			return(data.frame())

		scans <- object@scan[which(object@scan$plid == unique(object@xlink$peakid[xid])),]
		pre <- data.frame(scan.num=paste(scans$scanid,collapse=" "),
							pre.int=scans$pre.int[1],
							pre.mass=scans$mz[1]*scans$z[1]-scans$z[1]*(1.007825035-0.000549),
							pre.error=object@xlink$error[xid[1]],
							frag.cov=object@xlink$frag.cov[xid[1]],
							mods=object@xlink$mods[xid[1]])
		xlprot <- subset(object@prot,peakid==object@xlink$peakid[xid[1]])
		peps <- do.call("cbind",lapply(1:length(xpid),FUN=.peprow,object@xlpep[xpid,],xlprot))
		cbind(pre,peps)
	}))
	td
}
