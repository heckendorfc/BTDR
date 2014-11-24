\name{fragment.coverage}
\alias{fragment.coverage}
\title{
Plot fragment coverage
}
\description{
Plot fragment coverage based on BUPID results.
}
\usage{
fragment.coverage(data,file="cov.svg",columns=25,scale=2,peaklistid=1,color=T)
fragment.coverage.generic(data,sequence,file="cov.svg",columns=25,scale=2,color=T)
}
\details{
The data is the output from read.bupid. File is the name of the output file for the document. Columns is the number of columns to use for the sequence. Scale determines how large the image will be. Peaklistid is the peak list ID used for fragmentation. Color determines whether to use red/blue colors for fragments or all black.
The fragment coverage image can also be called using a generic non-bupid format. Data in the generic function takes the form data.frame(term=c("C","N"),num=c(1,50)), where term is the protein terminus the fragment is generated from, and num is the fragment number or length in amino acids.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns the name of the file created.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\examples{
#bupid version
infile <- "http://bumc-florida.bumc.bu.edu/BUPID_TD/cgi-bin/get_results.cgi?key=a1edb86d8f2d4cfcfd065755c5c725f8&ID=136&date=20140801"
data <- read.bupid(url=infile)
fragment.coverage(data,file="136_coverage.svg",columns=50)
#generic version
data <- data.frame(term=c("N","N","C","C"),num=c(1,2,1,2))
fragment.coverage.generic(data,"ACDEF",file="generic_coverage.svg",scale=3,color=F)
}
