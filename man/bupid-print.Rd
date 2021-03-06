% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.R
\docType{methods}
\name{bupid-print}
\alias{bupid-print}
\alias{head,bupid-method}
\alias{print,bupid-method}
\alias{tail,bupid-method}
\title{Display BUPID results.}
\usage{
\S4method{print}{bupid}(x, type = "overview")

\S4method{head}{bupid}(x, type = "overview", n = 6)

\S4method{tail}{bupid}(x, type = "overview", n = 6)
}
\arguments{
\item{x}{A bupid object}

\item{type}{The type of output to display. One of the strings returned by
c("overview","protein","fragment") or
paste("raw",slotNames(data),sep="-")}

\item{n}{The number of items to display}
}
\value{
Returns the information
}
\description{
BUPID results
}
\details{
Each result from BTDR packages will provide different outputs
}
\examples{
\dontrun{
server <- "http://bupid.bumc.bu.edu/cgi-bin/get_results.cgi"
infile <- "key=WBNqTswT5DPg3aDO&ID=320&date=20150309"
data <- read.bupid(url=paste(server,infile,sep="?"))
print(data,"overview")
head(data,"overview",10)
tail(data,"overview",2)
}

}

