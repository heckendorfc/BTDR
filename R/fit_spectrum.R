.peak.name <- function(data,plid){
	fid <- which(sapply(data$fit,FUN=function(x)if(x$prot$param$peaks$id==plid)T else F)==T)
	ret <- sapply(data$fit[[fid]]$results,FUN=.get.frag.name)
}

plot.spectrum <- function(data,peaklistid=1L){
	plx <- data$peaks[[peaklistid]]$m
	ply <- data$peaks[[peaklistid]]$intensity
	#pln <- .peak.name(data,peaklistid)
	#plc <- sapply(pln,.get.frag.color);
	df <- data.frame(mass=plx,intensity=ply,stringsAsFactors=F)
	ggplot(df,aes(x=mass,y=intensity)) +
	geom_segment(aes(xend=mass,yend=-Inf))+
	geom_segment(aes(xend=mass,yend=intensity))
}

plot.label.spectrum <- function(data,peaklistid=1L){
	#labels <- sapply(data$peaks[[peaklistid]],FUN=function(i)c()
	dp <- data$peaks[[peaklistid]]
	labels <- data.frame(mass=c(dp$mass[1],dp$mass[50]),intensity=c(dp$intensity[1],dp$intensity[50]),label=c("as","df"),color=c("red","blue"))
	geom_text(data=labels,aes(x=mass,y=intensity,label=label,colour=color,hjust=0,vjust=0))
}
