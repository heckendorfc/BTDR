fragment.term <- function(fragname){
	small <- substr(fragname,1,1)
	if(length(which(c("a","b","c")==small))>0)
		"N"
	else if(length(which(c("x","y","z")==small))>0)
		"C"
	else
		""
}

.get.frag.color <- function(frag,color=T){
	if(!color)
		return("#000")

	termpair <- data.frame(term=c("C","N",""),color=c("#00f","#f00","#000"),stringsAsFactors=F)
	termpair$color[which(termpair$term==frag)]
}

.get.path2 <- function(startx,starty,ldown1,ldown2,lover){
	paste0(list("M",startx,",",starty,
	"L",startx,",",ldown1,
	"M",startx,",",ldown1,
	"L",lover,",",ldown2,
	"Z"),collapse="")
}

.get.path <- function(startx,starty,ldown1,ldown2,lover,scale){
	width=scale/2;
	paste0(list("M",startx,",",starty,
	"L",startx,",",ldown1,
	"L",lover,",",ldown2,
	"L",lover,",",ldown2-width,
	"L",startx+width,",",ldown1-width,
	"L",startx+width,",",starty,
	"Z"),collapse="")
}

.get.frag.path <- function(res,params){
	si <- as.integer(res["num"])

	sx <- params$startx+0.5*params$xsp+((si%%params$numperline)*params$xsp);
	sy <- params$starty+(floor(si/params$numperline)*params$ysp);

	if(res["term"]=="N"){
		ldown1 <- sy-params$height
		ldown2 <- ldown1-params$height*0.5
		lover <- sx-params$width
	}
	else{
		ldown1 <- sy+params$height
		ldown2 <- ldown1+params$height*0.5
		lover <- sx+params$width
	}

	.get.path(sx,sy,ldown1,ldown2,lover,params$scale)
}

fragment.coverage.generic <- function(data,sequence,file="cov.svg",columns=25L,scale=2,peaklistid=1L,color=T){
	params <- data.frame(scale=scale,starty=20,startx=10,numperline=columns)
	params$xsp <- 15*params$scale
	params$ysp <- 15*params$scale
	params$width <- params$xsp/6
	params$height <- params$ysp/3

	cx <- sapply(0:(nchar(sequence)-1),FUN=function(i)(i%%params$numperline)*params$xsp+params$startx)
	cy <- sapply(0:(nchar(sequence)-1),FUN=function(i)floor(i/params$numperline)*params$ysp+params$starty)

	tx <- max(cx)+params$startx+params$xsp
	ty <- max(cy)+params$starty+params$ysp

	xml <- xmlTree("svg",attrs=c(viewbox=paste0(c(0,0,tx,ty),collapse=" ")), namespace=c("http://www.w3.org/2000/svg",xsi="http://www.w3.org/2001/XMLSchema-instance"), dtd=c("svg","-//W3C//DTD SVG 1.1//EN","http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"))
	xml$addNode("text",sequence,attrs=c(x=paste0(cx,collapse=" "),y=paste0(cy,collapse=" "),"text-anchor"="middle","font-family"="sans-serif","font-size"=10*params$scale,fill="#000","dominant-baseline"="middle"))

	for(i in 1:(nrow(data))){
			fcolor <- .get.frag.color(data[i,"term"],color=color)
			xml$addNode("path",attrs=c(d=.get.frag.path(data[i,],params),stroke=fcolor,fill=fcolor))
	}

	saveXML(xml,file,indent=T)
}

fragment.coverage.convert.input <- function (data,peaklistid=1L){
	fi <- which(sapply(1:length(data$fit),FUN=function(df)if(data$fit[[df]]$prot$param$peaks$id==peaklistid)df else 0)>0)
	dfi <- data$fit[[fi]]
	ret <- sapply(dfi$results,FUN=function(res){
		term <- fragment.term(res$frag)
		if(is.null(term) || term==""){
			return(c(term=NULL,num=-1L))
		}
		num <- if(term=="N") res$ion$len-1 else res$ion$start-1
		c(term=term,num=num);
	})
	ret <- t(ret)
	bad <- which(ret[,"term"] == NULL)
	if(length(bad)>0)
		ret <- ret[-bad,]
	ret
}

fragment.coverage <- function(data,file="cov.svg",columns=25L,scale=2,peaklistid=1L,color=T){
	gdata <- fragment.coverage.convert.input(data)
	fragment.coverage.generic(gdata,data$prot[[1]]$seq,file,columns,scale,peaklistid,color)
}
