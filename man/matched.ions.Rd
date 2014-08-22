\name{matched.ions}
\alias{matched.ions}
\title{
Create a matrix of fragment-peak assignments.
}
\description{
Create a matrix of fragment-peak assignments.
}
\usage{
matched.ions(data,peaklistid=1L)
}
\details{
Data is the output from read.bupid. Peaklistid is the peak list id to use for the assignment data.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns the matrix of assignments.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\examples{
infile <- "http://bumc-florida.bumc.bu.edu/BUPID_TD/cgi-bin/get_results.cgi?key=a1edb86d8f2d4cfcfd065755c5c725f8&ID=136&date=20140801"
data <- read.bupid(url=infile)
matched.ions(data)
}

