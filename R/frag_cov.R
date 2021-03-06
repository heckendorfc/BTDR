#' Plot fragment coverage
#' 
#' These functions generate a map of fragmentation sites within a given
#' protein sequence.
#' 
#' @param data
#' a bupid object; output from read.bupid
#' @param file
#' the output file name
#' @param columns
#' the number of columns (residues per line) to use for the sequence
#' @param scale
#' magnification factor for the image
#' @param color
#' a length 3 character vector indicating the color C-term, N-term, and other
#' fragments should be colored respectively. Also valid are NULL to make them
#' all black or a length 1 character vector to make them all that color.
#' @param marksite
#' site numbers to place a marker at
#' 
#' @return Returns the name of the file created.
#' @examples
#' \dontrun{
#' #bupid version
#' server <- "http://bupid.bumc.bu.edu/cgi-bin/get_results.cgi"
#' infile <- "key=WBNqTswT5DPg3aDO&ID=320&date=20150309"
#' data <- read.bupid(url=paste(server,infile,sep="?"))
#' fragment.coverage(data,file="136_coverage.svg",columns=50)
#'
#' #generic version
#' data <- data.frame(term=c("N","N","C","C"),num=c(1,2,1,2))
#' fragment.coverage.generic(data,"ACDEF",file="generic_coverage.svg",scale=3,color=F)
#' }
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

.get.frag.color <- function(frag,color){
	if(length(color)==0)
		return("#000")
	if(length(color)==1)
		return(color)

	termpair <- data.frame(term=c("C","N",""),color=color,stringsAsFactors=F)
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

.param.coord <- function(params,si){
	sx <- params$scale*3+params$startx+0.5*params$xsp+((si%%params$numperline)*params$xsp);
	sy <- params$starty+(floor(si/params$numperline)*params$ysp);
	data.frame(sx=sx,sy=sy)
}

.get.frag.path <- function(res,params,seqlen){
	si <- as.integer(res["num"])
	ns <- which(res["term"] == "N")
	cs <- which(res["term"] == "C")
	si <- c(si[ns]-1,seqlen-si[cs]-1)

	coord <- .param.coord(params,si)
	sx <- coord$sx
	sy <- coord$sy

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

.fragment.coverage.convert.input <- function (data){
	term <- fragment.term(data$frag)
	t1 <- which(term %in% c("N","C"))
	ret <- data.frame(term=term[t1],num=data$ion.len[t1])
	ret
}

#' fragment.coverage plots fragment coverage based on BUPID results.
#'
#' @rdname fragment.coverage
#' @export fragment.coverage
fragment.coverage <- function(data,file="cov.svg",columns=25L,scale=2,color=c("#00f","#f00","#000"),marksite=NULL){
	pn <- unique(data@prot$name)
	if(length(pn)>1){
		sapply(pn,FUN=function(n){
			pids <- which(data@prot$name==n)
			fids <- get_unique_prot_id(data@fit$peak.id,data@fit$protid) %in%
										  get_unique_prot_id(data@prot$peakid[pids],data@prot$protid[pids])

			gdata <- .fragment.coverage.convert.input(data@fit[fids,])
			fragment.coverage.generic(gdata,as.character(data@prot$seq[1]),paste(strsplit(as.character(n),"[|]")[[1]][1],file,sep="."),columns,scale,color,marksite)
		})
	} else{
		gdata <- .fragment.coverage.convert.input(data@fit)
		fragment.coverage.generic(gdata,as.character(data@prot$seq[1]),file,columns,scale,color,marksite)
	}
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
fragment.coverage.generic <- function(data,sequence,file="cov.svg",columns=25L,scale=2,color=c("#00f","#f00","#000"),marksite=NULL){
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
	ty <- max(cy)+params$starty

	root <- xml_new_root(xml_dtd("svg","-//W3C//DTD SVG 1.1//EN","http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"))
	xml <- xml_add_child(root,"svg",viewbox=paste0(c(0,0,tx,ty),collapse=" "),width=tx,height=ty, xmlns="http://www.w3.org/2000/svg","xmlns:xsi"="http://www.w3.org/2001/XMLSchema-instance")
	xml_add_child(xml,"text") %>%
		xml_add_child("tspan",sequence,x=paste0(cx,collapse=" "),y=paste0(cy,collapse=" "),"font-family"="sans-serif","font-size"=paste(10*params$scale,"px",sep=""),fill="#000",dy=paste(rep(params$scale*3,nchar(sequence)),collapse=" "))

	if(nrow(data)>0){
		for(i in 1:(nrow(data))){
			fcolor <- .get.frag.color(data[i,"term"],color=color)
			xml_add_child(xml,"path",d=.get.frag.path(data[i,],params,nchar(sequence)),stroke=fcolor,fill=fcolor)
		}
	}

	if(length(marksite)>0){
		coord <- .param.coord(params,marksite)
		coord$sx <- coord$sx - params$xsp/2
		coord$sy <- coord$sy - params$height*1.25

		#xml_add_child(xml,"text") %>%
			#xml_add_child("tspan",rep("*",length(marksite)),x=paste0(coord$sx,collapse=" "),y=paste0(coord$sy,collapse=" "),"font-family"="sans-serif","font-size"=paste(5*params$scale,"px",sep=""),fill="#F00",dy=paste(rep(params$scale*3,length(marksite)),collapse=" ")) # x-coord is slightly off

		xml_add_child(xml,"circle",
					  cx=paste0(coord$sx,collapse=" "),
					  cy=paste0(coord$sy,collapse=" "),
					  fill="#F00",
					  r=params$height*0.25)
	}

	write_xml(root,file,options="as_xml")
}
