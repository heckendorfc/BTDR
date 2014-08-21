\name{fragment.coverage}
\alias{fragment.coverage}
\title{
Plot fragment coverage
}
\description{
Plot fragment coverage based on BUPID results.
}
\usage{
fragment.coverage(data,file="cov.svg",columns=25,peaklistid=1)
}
\details{
The data is the output from read.bupid. File is the name of the output file for the document. Columns is the number of columns to use for the sequence. Peaklistid is the peak list ID used for fragmentation.
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
infile <- "http://bumc-florida.bumc.bu.edu/BUPID_TD/cgi-bin/get_results.cgi?key=a1edb86d8f2d4cfcfd065755c5c725f8&ID=136&date=20140801"
data <- read.bupid(url=infile)
fragment.coverage(data,file="136_coverage.svg",columns=50)
}

