#' Plot mass errors for fragment assignments.
#'
#' Generates a plot showing mass error vs. ion mass.
#'
#' @param data
#' output from read.bupid
#' @param recalibrate
#' adjust mass errors so they are centered around zero
#'
#' @return Returns the ggplot object representing the plot.
#'
#' @examples
#' server <- "http://bupid.bumc.bu.edu/BUPID_TD/cgi-bin/get_results.cgi"
#' infile <- "key=WBNqTswT5DPg3aDO&ID=320&date=20150309"
#' data <- read.bupid(url=paste(server,infile,sep="?"))
#' png(filename="errors.png",width=300,height=100) #Configure a graphics device
#' fragment.mass.error(data)
#' dev.off()
#'
#' @export fragment.mass.error
fragment.mass.error <- function(data,recalibrate=F){
	err <- .ppm.error(data@fit)
	vmass <- data@fit$ion.mass
	vcolor <- .fragment.color(data@fit)

	if(recalibrate){
		model <- lm("err~mass",data.frame(err=err,mass=vmass))
		err <- err - (vmass*model$coefficients[2]+model$coefficients[1])
	}

	df <- data.frame(ppmError=err,mass=vmass,color=vcolor)

	ggplot(df,aes(x=mass,y=ppmError))+
	geom_point(aes(colour=color))+
	geom_hline(aes(colour=black),yintercept=0)+ 
	scale_colour_manual(values=c("#0000FF", "#FF0000","#444444"),labels=c("C-term","N-term","Other"),name="Fragment")+
	theme(legend.justification=c(1,1), legend.position=c(1,1))
}
