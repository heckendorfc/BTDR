% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mzidentml.R
\name{write.mzid}
\alias{write.mzid}
\title{Write results as mzIdentML.}
\usage{
write.mzid(data, file, inputfile = NULL)
}
\arguments{
\item{data}{output from read.bupid}

\item{file}{the name of the file you want to save the mzIdentML results as.}

\item{inputfile}{the name of file passed to read.bupid.}
}
\value{
Returns the name of the file generated.
}
\description{
Convert an object created with read.bupid to mzIdentML.
}
\examples{
\dontrun{
server <- "http://bupid.bumc.bu.edu/cgi-bin/get_results.cgi"
infile <- "key=WBNqTswT5DPg3aDO&ID=320&date=20150309"
data <- read.bupid(url=paste(server,infile,sep="?"))
write.mzid(data,"data.mzid",infile)
}

}
\seealso{
\code{\link{read.bupid}}
}

