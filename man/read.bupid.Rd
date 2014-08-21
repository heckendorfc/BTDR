\name{read.bupid}
\alias{read.bupid}
\title{
Read BUPID results.
}
\description{
Read BUPID results and create and object from them.
}
\usage{
write.mzid(data,file,inputfile=NULL)
}
\details{
The data can be passed either as the name of a file on the local computer or a URL on the BUPID server containing the results.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns the results as an R object.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\examples{
infile <- "http://bumc-florida.bumc.bu.edu/BUPID_TD/results.html?key=a1edb86d8f2d4cfcfd065755c5c725f8&ID=136&date=20140801"
data <- read.bupid(infile)
}

