fragment.mass.error <- function(data,peaklistid=1L){
	resterm <- sapply(data$fit[[peaklistid]]$results,FUN=function(r)fragment.term(r$frag))
	resi <- c(which(resterm=="C"),which(resterm=="N"))
	res <- lapply(1:length(resi),FUN=function(i)data$fit[[peaklistid]]$results[[i]])
	err <- sapply(res,FUN=function(r)r$err/r$ion$mass*1e6)
	vmass <- sapply(res,FUN=function(r)r$ion$mass)
	vcolor <- sapply(1:length(resi),FUN=function(i)if(resterm[i]=="N") "red" else "blue")
	df <- data.frame(ppmError=err,mass=vmass,color=vcolor)
	ggplot(df,aes(x=mass,y=ppmError)) + geom_point(colour=df$color) + geom_hline(aes(colour=black),yintercept=0)
}
