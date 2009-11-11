\name{demographics}
\alias{demographics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Grabs specified demographic meta-data from the sp objects.
}
\description{
\code{demographics} allows the user to pull out one or more demographic variables at several different geographic levels.
}
\usage{
demographics(dem = "pop2000", state, statefips=FALSE, level = c("tract", "blk", "blkgrp", "cdp","msa","county"), msaname=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dem}{ Character string or vector. Must be the actual name of demographic variables attached to \code{census2000} objects. Default \code{dem} = "pop2000"
}
  \item{state}{
This has to be a string and can either be the full name (e.g. "oregon"), the abbreviation (e.g. "or"), or the FIPS code (e.g. "41")-- note that if you are using the FIPS code you  have to change \code{statefips} to \code{TRUE}. This variable is insensitive to case.
}
  \item{statefips}{
logical: are you providing \code{state} with a FIPS code instead of a name. By default, \code{statefips}=\code{FALSE}.
}
  \item{level}{
Takes in one of six values: "tract", "blk", "blkgrp", "cdp","msa","county". This defines the geographic level of data for the county.
}
  \item{msaname}{Takes the value of a string. This can either be in conjunction with the variable \code{state} or not. Case 1: Full MSA name (does not require state to be defined) (e.g.  "Abilene, TX MSA"). Case 2: takes one of the city names of the MSA and the state which contains the MSA  (e.g. \code{msaname}="Albany" and \code{state}= "NY"; note that in this case the city must be capitalized followed by lowercase letters and the state must be in capitals). See also \code{\link{MSA}}.
}
}
%%\details{}
\value{
An object of class \code{\link[base:data.frame]{data.frame}}.
}
\references{
Census 2000 Summary File 1 [name of state1 or United States]/prepared by the U.S. Census 
Bureau, 2001. \cr
\url{http://www.census.gov/prod/cen2000/doc/sf1.pdf}
}
\author{Zack W. Almquist \email{almquist@uci.edu}}
%\note{}
\section{Warning }{You must have the packages \code{census2000blkgrp} and \code{census2000blk} installed to use levels "blkgrp" and "blk" respectively.
} 

\seealso{
\code{\link{county}}, \code{\link{MSA}}, \code{\link{city}}
}
\examples{

##MSA example
bayarea<-demographics(dem=c("pop2000","white","black"),"CA",level="msa",msaname="San Francisco")

## Print it out
bayarea
##Note that the columns are labeled by the county FIPS code.

##CDP example
ca.cdp<-demographics(dem=c("pop2000","white","black"),"CA",level="cdp")

##Print out in alphabetic order
ca.cdp[order(rownames(ca.cdp)),]

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{demographics}
\keyword{uscensus}
\keyword{census}% __ONLY ONE__ keyword per line
