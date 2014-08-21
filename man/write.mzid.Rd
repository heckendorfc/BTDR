\name{write.mzid}
\alias{write.mzid}
\title{
Write results as mzIdentML.
}
\description{
Convert an object created with read.bupid to mzIdentML.
}
\usage{
write.mzid(data,file,inputfile=NULL)
}
\details{
Convert an object created with read.bupid to mzIdentML.
Data is the object returned from read.bupid. File is the name of the file you want to save the mzIdentML results as. Inputfile is the name of file passed to read.bupid.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns the name of the file generated.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\seealso{
\code{\link{read.bupid}}
}
\examples{
infile <- "http://bumc-florida.bumc.bu.edu/BUPID_TD/results.html?key=a1edb86d8f2d4cfcfd065755c5c725f8&ID=136&date=20140801"
data <- read.bupid(infile)
write.mzid(data,"data.mzid",infile)
}
