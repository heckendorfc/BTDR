\name{fragment.mass.error}
\alias{fragment.mass.error}
\title{
Plot mass errors for fragment assignments.
}
\description{
Plot mass errors for fragment assignments.
}
\usage{
fragment.mass.error(data,peaklistid=1L)
}
\details{
Data is the output from read.bupid. Peaklistid is the peak list id to use for the assignment data.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns the ggplot object representing the plot.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\examples{
infile <- "http://bumc-florida.bumc.bu.edu/BUPID_TD/cgi-bin/get_results.cgi?key=a1edb86d8f2d4cfcfd065755c5c725f8&ID=136&date=20140801"
data <- read.bupid(url=infile)
png(filename="errors.png",width=300,height=100) #Configure a graphics device
fragment.mass.error(data)
dev.off()
}


