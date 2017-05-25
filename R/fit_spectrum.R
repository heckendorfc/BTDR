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
#' @param unassigned
#' display unassigned peaks behind the labelled spectrum
#' @param base_size
#' base text size for axis labels
#' @param mz
#' TRUE to plot in m/z, otherwise mass
#' 
#' @return Returns the ggplot object
#'
#' @examples
#' \dontrun{
#' server <- "http://bupid.bumc.bu.edu/cgi-bin/get_results.cgi"
#' infile <- "key=WBNqTswT5DPg3aDO&ID=320&date=20150309"
#' data <- read.bupid(url=paste(server,infile,sep="?"))
#' 
#' # plot the entire spectrum
#' spectrum.plot(data)
#' 
#' # plot peaks between 500 Da and 600 Da with labeled assignments
#' spectrum.label.plot(data,c(500,600))
#' 
#' # again with a simple theme
#' spectrum.label.plot(data,c(500,600)) + spectrum.theme.simple()
#' }
#' 
#' @name spectrum.plot
NULL

#' spectrum.plot.generic plots the deconvoluted peak list as a mass spectrum
#' 
#' @return Returns the ggplot object
#' 
#' @rdname spectrum.plot
#' @export spectrum.plot.generic
spectrum.plot.generic <- function(mass,intensity,massrange=c(0,Inf),color="#000000"){
	inds <- which(mass>=massrange[1] & mass<=massrange[2])
	plx <- mass[inds]
	ply <- intensity[inds]
	df <- data.frame(mass=plx,intensity=ply,stringsAsFactors=F)
	ggplot(df,aes(x=mass,y=intensity)) +
	geom_segment(aes(xend=mass,yend=-Inf),colour=color)
}

#' spectrum.plot plots the deconvoluted peak list as a mass spectrum
#' 
#' @return Returns the ggplot object
#' 
#' @rdname spectrum.plot
#' @export spectrum.plot
spectrum.plot <- function(data,massrange=c(0,Inf),color="#000000"){
	spectrum.plot.generic(data@decon$mass,data@decon$intensity,massrange,color)
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

#' spectrum.label.plot adds fragment assignment labels to spectrum.plot
#' 
#' @return Returns the ggplot object
#' 
#' @rdname spectrum.plot
#' @export spectrum.label.plot
spectrum.label.plot <- function(data,massrange=c(0,Inf),unicode=TRUE,unassigned=TRUE,mz=FALSE){
	fp <- .expand.fit.counts(data)
	fp <- fp[which(fp$peak.mass>=massrange[1] & fp$peak.mass<=massrange[2]),]
	vcolor <- .fragment.color(fp)
	if(mz){
		labels <- data.frame(mass=.mass.to.mz(fp$peak.mass,fp$peak.z),intensity=fp$peak.intensity,label=.get.frag.name(fp),color=vcolor)
	} else {
		labels <- data.frame(mass=fp$peak.mass,intensity=fp$peak.intensity,label=.get.frag.name(fp),color=vcolor)
	}
	if(length(unique(fp$protid))>1 && length(unique(fp$peak.id))==1){ #assume xlink?
		labels$label <- paste0(labels$label,paste0("^",fp$protid+1))
	}
	if(unicode){
		labels$label <- sub("[+]1","'",labels$label)
		labels$label <- sub("-1","\\u2022",labels$label)
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
	if(unassigned){
		if(mz){
			mass <- .mass.to.mz(data@decon$mass,data@decon$z)
		} else {
			mass <- data@decon$mass
		}
		gp <- spectrum.plot.generic(mass,data@decon$intensity,massrange,color="#888888")
	} else {
		gp <- ggplot()
	}
	gp <- gp + geom_segment(data=labels,aes(x=mass,xend=mass,y=intensity,yend=-Inf,colour=color))+
	geom_text(data=labels,parse=T,aes(x=mass,y=intensity,label=label,colour=color,hjust=0,vjust=0))+
	scale_colour_manual(values=c("blue"="#0000FF", "red"="#FF0000", "black"="#444444"),labels=c("blue"="C-term","red"="N-term","black"="Other"),name="Fragment")+
	theme(legend.justification=c(1,1), legend.position=c(1,1))
	if(mz)
		gp <- gp + labs(x="m/z")
	gp
}

#' spectrum.theme.simple modifies the spectrum plot to use black axis labels and no background
#' 
#' @return Returns the ggplot theme
#' 
#' @rdname spectrum.plot
#' @export spectrum.theme.simple
spectrum.theme.simple <- function(base_size = 12) {
	theme(panel.background =  element_rect(fill = NA, colour = "black", size = 0.25),
	axis.text.x = element_text(colour = "black",size = base_size * 0.8 , lineheight = 0.9, vjust = 1),
	axis.text.y = element_text(colour = "black",size = base_size * 0.8, lineheight = 0.9, hjust = 1),
	panel.grid.major = element_blank(),
	panel.grid.minor=element_blank())
}
