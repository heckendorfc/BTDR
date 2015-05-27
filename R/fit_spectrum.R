#' Plot spectra
#'
#' These functions plot the deconvoluted spectra and fragment assignments
#'
#' @param data
#' output from read.bupid
#' @param massrange
#' a vector describing the lower and upper bounds of the mass range to plot
#' @param color
#' the color to use for unlabeled peaks
#' @param unicode
#' allow unicode characters in labels
#' 
#' @return Returns the ggplot object
#'
#' @examples
#' server <- "http://bumc-florida.bumc.bu.edu/BUPID_TD/cgi-bin/get_results.cgi"
#' infile <- "key=WBNqTswT5DPg3aDO&ID=320&date=20150309"
#' data <- read.bupid(url=paste(server,infile,sep="?"))
#' 
#' # plot the entire spectrum
#' plot.spectrum(data)
#' 
#' # plot peaks between 500 Da and 600 Da with labeled assignments
#' plot.label.spectrum(data,c(500,600))
#' 
#' # again with a simple theme
#' plot.label.spectrum(data,c(500,600)) + plot.theme.simple()
#' 
#' @name plot.spectrum
NULL

#' plot.spectrum plots the deconvoluted peak list as a mass spectrum
#' 
#' @return Returns the ggplot object
#' 
#' @rdname plot.spectrum
#' @export plot.spectrum
plot.spectrum <- function(data,massrange=c(0,Inf),color="#000000"){
	inds <- which(data@decon$mass>=massrange[1] & data@decon$mass<=massrange[2])
	plx <- data@decon$mass[inds]
	ply <- data@decon$intensity[inds]
	#pln <- .peak.name(data,peaklistid)
	#plc <- sapply(pln,.get.frag.color);
	df <- data.frame(mass=plx,intensity=ply,stringsAsFactors=F)
	ggplot(df,aes(x=mass,y=intensity)) +
	geom_segment(aes(xend=mass,yend=-Inf),colour=color)
	#geom_segment(aes(xend=mass,yend=intensity))
}

.fragment.color <- function(fit){
	resterm <- fragment.term(fit$frag)

	nt <- which(resterm=="N")
	ct <- which(resterm=="C")
	vcolor <- rep("black",length(resterm))
	vcolor[nt] <- "red"
	vcolor[ct] <- "blue"

	vcolor
}

#' plot.label.spectrum adds fragment assignment labels to plot.spectrum
#' 
#' @return Returns the ggplot object
#' 
#' @rdname plot.spectrum
#' @export plot.label.spectrum
plot.label.spectrum <- function(data,massrange=c(0,Inf),unicode=TRUE){
	#labels <- sapply(data$peaks[[peaklistid]],FUN=function(i)c()
	inds <- which(data@decon$mass>=massrange[1] & data@decon$mass<=massrange[2])
	dp <- data@decon[inds,]
	inds <- which(data@fit$peak.mass>=massrange[1] & data@fit$peak.mass<=massrange[2])
	fp <- data@fit[inds,]
	#labels <- data.frame(mass=c(dp$mass[1],dp$mass[50]),intensity=c(dp$intensity[1],dp$intensity[50]),label=c("as","df"),color=c("red","blue"))
	vcolor <- .fragment.color(fp)
	labels <- data.frame(mass=fp$peak.mass,intensity=fp$peak.intensity,label=.get.frag.name(fp),color=vcolor)
	if(unicode){
		labels$label <- sub("[+]1","'",labels$label)
		labels$label <- sub("-1","•",labels$label)
		labels$label <- gsub("\\[|]","",labels$label)
		Encoding(labels$label) <- "UTF-8"
	}
	#unique / diff ?
	isl <- order(labels$intensity,decreasing=T)
	unl <- !duplicated(cbind(round(labels$mass[isl]),labels$label[isl]))
	labels <- labels[isl[unl],]
	#plotdata <- data.frame(mass=c(fp$peak.mass[nt],fp$peak.mass[ct]),
						   #intensity=c(fp$peak.intensity[nt],fp$peak.intensity[ct]),
						   #color=vcolor)
	plot.spectrum(data,massrange,color="#888888")+
	geom_segment(data=labels,aes(x=mass,xend=mass,y=intensity,yend=-Inf,colour=color))+
	geom_text(data=labels,aes(x=mass,y=intensity,label=label,colour=color,hjust=0,vjust=0))+
	scale_colour_manual(values=c("#0000FF", "#FF0000","#444444"),labels=c("C-term","N-term","Other"),name="Fragment")+
	theme(legend.justification=c(1,1), legend.position=c(1,1))
}

#' plot.theme.simple modifies the spectrum plot to use black axis labels and no background
#' 
#' @return Returns the ggplot theme
#' 
#' @rdname plot.spectrum
#' @export plot.theme.simple
plot.theme.simple <- function(base_size = 12) {
	theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.25),
	axis.text.x = element_text(colour = "black",size = base_size * 0.8 , lineheight = 0.9, vjust = 1),
	axis.text.y = element_text(colour = "black",size = base_size * 0.8, lineheight = 0.9, hjust = 1),
	panel.grid.major = element_blank(),
	panel.grid.minor=element_blank())
}
