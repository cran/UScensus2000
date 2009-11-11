\name{MSA}
\alias{MSA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Grab one MSA from a given state.
}
\description{
\code{MSA} allows the user to pull out the polygon and metadata of one MSA from any given state for any of three levels: tract, blockgroup, or block.
}
\usage{
MSA(msafips = NULL, msaname = NULL, state=NULL ,level = c("tract", "blk", "blkgrp"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{msafips}{Takes the value of a string. This needs to be the four digit MSA FIPS code (e.g. "0040" of Texas).
}
  \item{msaname}{Takes the value of a string. This can either be in conjunction with the variable \code{state} or not. Case 1: Full MSA name (does not require state to be defined) (e.g.  "Abilene, TX MSA"). Case 2: takes one of the city names of the MSA and the state which contains the MSA  (e.g. \code{msaname}="Albany" and \code{state}= "NY"; note that in this case the city must be capitalized followed by lowercase letters and the state must be in capitals). 
}
\item{state}{
Takes in a two character string. Specifically this takes in a state abbreviation in capitals in conjunction with \code{msaname}, see above for more details. 
}
  \item{level}{
Takes in one of three values: "tract", "blk",  or "blkgrp". This defines the geographic level of data for the MSA.
}

}
%%\details{}
\value{
An object of class \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}}.
}
\references{
Census 2000 Summary File 1 [name of state1 or United States]/prepared by the U.S. Census 
Bureau, 2001. \cr
\url{http://www.census.gov/prod/cen2000/doc/sf1.pdf}
}
\author{Zack W. Almquist \email{almquist@uci.edu}
}
%%\note{}

\section{Warning }{You must have the packages \code{UScensus2000blkgrp} and \code{UScensus2000blk} installed to use levels "blkgrp" and "blk" respectively.} 


\seealso{
\code{\link{county}}
}
\examples{
## Load the data files for MSA names and MSA FIPS codes
data(MSAnames)
data(MSAfips)

## Save the FIPS code for Abilene, TX MSA
ab.fips<-MSAfips$msa.cmsa.fips[1]

Abilene<-MSA(msafips=ab.fips,level="tract")

##Use the msaname alternative
portland<-MSA(msaname="Portland",state="OR",level="tract")


##Plot Portland
plot(portland)
title("Portland MSA, OR 2000")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{shapefiles}
\keyword{demography}
\keyword{msa}% __ONLY ONE__ keyword per line
