\name{city}
\alias{city}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Grab  one or more CDP(s) from a given state
}
\description{
\code{city} allows the user to pull out the polygon and metadata of one or more CDPs for any given state by name.
}
\usage{
city(name, state, statefips = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{Takes the value of a string or string vector and has to be the exact name or names of CDP(s). (If you are unsure of the exact name a quick way to find it is to load the \code{library(UScensus2000cdp)} and pull out the list of names for the state you are interest in (see example).
}
  \item{state}{
This has to be a string and can either be the full name (e.g. "oregon"), the abbreviation (e.g. "or"), or the FIPS code (e.g. "41")-- note that if you are using the FIPS code you  have to change \code{statefips} to \code{TRUE}.
}
  \item{statefips}{
logical: are you providing \code{state} with a FIPS code instead of a name. By default, \code{statefips}=\code{FALSE}.
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
\author{
Zack W. Almquist \email{almquist@uci.edu}
}
%%\note{ }

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{}
\examples{
##Load oregon.cdp data 
data(oregon.cdp)

##look at the list of oregon CDP names in alphabetic order
oregon.cdp$name[order(oregon.cdp$name)]

##grab the CDP of Portland, OR
portland<-city(name="portland",state="or")


##plot the polygon of Portland, OR
plot(portland)
title("Portland, OR")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{shapefiles}
\keyword{CDP}
\keyword{uscensus}
\keyword{demography}
\keyword{census}% __ONLY ONE__ keyword per line
