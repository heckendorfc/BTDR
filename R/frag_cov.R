#' Plot fragment coverage
#' 
#' These functions generate a map of fragmentation sites within a given
#' protein sequence.
#' 
#' @param data
#' output from read.bupid
#' @param fitid
#' peak fit result index to process
#' @param file
#' the output file name
#' @param columns
#' the number of columns (residues per line) to use for the sequence
#' @param scale
#' magnification factor for the image
#' @param color
#' boolean value with true indicating the fragments should be coloured red/blue
#' and false making them all black.
#'
#' 
#' @return Returns the name of the file created.
#' @examples
#' #bupid version
#' server <- "http://bumc-florida.bumc.bu.edu/BUPID_TD/cgi-bin/get_results.cgi"
#' infile <- "key=WBNqTswT5DPg3aDO&ID=320&date=20150309"
#' data <- read.bupid(url=paste(server,infile,sep="?"))
#' fragment.coverage(data,file="136_coverage.svg",columns=50)
#'
#' #generic version
#' data <- data.frame(term=c("N","N","C","C"),num=c(1,2,1,2))
#' fragment.coverage.generic(data,"ACDEF",file="generic_coverage.svg",scale=3,color=F)
#' 
#' @name fragment.coverage
NULL

fragment.term <- function(fragname){
	ret <- rep("",length(fragname))
	small <- substr(fragname,1,1)
	ret[small %in% c("a","b","c")] <- "N"
	ret[small %in% c("x","y","z")] <- "C"
	ret
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

.get.frag.path <- function(res,params,seqlen){
	si <- as.integer(res["num"])
	ns <- which(res["term"] == "N")
	cs <- which(res["term"] == "C")
	si <- c(si[ns]-1,seqlen-si[cs]-1)

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

fragment.coverage.convert.input <- function (data,fitid=1L){
	fi <- which(sapply(1:length(data$fit),FUN=function(df)if(data$fit[[df]]$prot$param$peaks$id==fitid)df else 0)>0)
	dfi <- data$fit[[fi]]
	#ret <- do.call("rbind",lapply(dfi$results,FUN=function(res){
	ret <- ldply(dfi$results,.fun=function(res){
		term <- fragment.term(res$frag)
		if(is.null(term) || term==""){
			return(data.frame(term="",num=-1L))
		}
		num <- res$ion$len
		#num <- if(term=="N") res$ion$len else res$ion$start-1
		data.frame(term=term,num=num);
	})
	#ret <- t(ret)
	bad <- which(ret[,"term"]=="")
	if(length(bad)>0)
		ret <- ret[-bad,]
	ret
}

#' fragment.coverage plots fragment coverage based on BUPID results.
#'
#' @rdname fragment.coverage
#' @export fragment.coverage
fragment.coverage <- function(data,file="cov.svg",columns=25L,scale=2,fitid=1L,color=T){
	gdata <- fragment.coverage.convert.input(data,fitid)
	fragment.coverage.generic(gdata,data$prot[[1]]$seq,file,columns,scale,color)
}

#' The fragment coverage image can also be called using a generic
#' non-bupid format. Data in the generic function takes the form
#' data.frame(term=c("C","N"),num=c(1,50)), where term is the protein
#' terminus the fragment is generated from, and num is the fragment
#' number or length in amino acids.
#' 
#' @param sequence
#' protein sequence used in assignments
#'
#' @rdname fragment.coverage
#' @export fragment.coverage.generic
fragment.coverage.generic <- function(data,sequence,file="cov.svg",columns=25L,scale=2,color=T){
	params <- data.frame(scale=scale,numperline=columns)
	params$xsp <- 15*params$scale
	params$ysp <- 15*params$scale
	params$width <- params$xsp/6
	params$height <- params$ysp/3
	params$starty <- 5+params$height*1.5
	params$startx <- 5*scale

	cx <- sapply(0:(nchar(sequence)-1),FUN=function(i)(i%%params$numperline)*params$xsp+params$startx)
	cy <- sapply(0:(nchar(sequence)-1),FUN=function(i)floor(i/params$numperline)*params$ysp+params$starty)

	tx <- max(cx)+params$startx+params$xsp
	ty <- max(cy)+params$starty+params$ysp

	xml <- xmlTree("svg",attrs=c(viewbox=paste0(c(0,0,tx,ty),collapse=" ")), namespaces=c("http://www.w3.org/2000/svg",xsi="http://www.w3.org/2001/XMLSchema-instance"), dtd=c("svg","-//W3C//DTD SVG 1.1//EN","http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"))
	xml$addNode("text",sequence,attrs=c(x=paste0(cx,collapse=" "),y=paste0(cy,collapse=" "),"text-anchor"="middle","font-family"="sans-serif","font-size"=10*params$scale,fill="#000","dominant-baseline"="middle"))

	for(i in 1:(nrow(data))){
			fcolor <- .get.frag.color(data[i,"term"],color=color)
			xml$addNode("path",attrs=c(d=.get.frag.path(data[i,],params,nchar(sequence)),stroke=fcolor,fill=fcolor))
	}

	saveXML(xml,file,indent=T)
}
