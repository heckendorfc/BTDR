\name{matched.clusters}
\alias{matched.clusters}
\title{
Create a matrix of fragment-cluster assignments.
}
\description{
Create a matrix of fragment-cluster assignments.
}
\usage{
matched.clusters(data,peaklistid=1L)
}
\details{
Data is the output from read.bupid. Peaklistid is the peak list id to use for the assignment data.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns the matrix of assignments along with their original m/z and charge state values as seen in the raw spectra.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\examples{
infile <- "http://bumc-florida.bumc.bu.edu/BUPID_TD/cgi-bin/get_results.cgi?key=a1edb86d8f2d4cfcfd065755c5c725f8&ID=136&date=20140801"
data <- read.bupid(url=infile)
mc <- matched.clusters(data)
write.csv(mc,"matched_clusters.csv")
}
