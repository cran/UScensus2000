\name{poly.clipper}
\alias{poly.clipper}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Grab the block, block group, or tracts which are contained within a given CDP.
}
\description{
\code{poly.clipper} allows the user to pull out all the blocks, block groups, or tracts that fall within the boundary of a given CDP and where the above grouping falls outside the boundaries  \code{poly.clipper} will output the intersection of the two polygons and estimate the demographic variables based on the area of the intersection.
}
\usage{
poly.clipper(name, state, statefips = FALSE, level = c("tract", "blk", "blkgrp"), bb.epsilon = 0.006) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{Takes in a string. This must be the name of CDP for a given state.
}
  \item{state}{
This has to be a string and can either be the full name (e.g. "oregon"), the abbreviation (e.g. "or"), or the FIPS code (e.g. "41")-- note that if you are using the FIPS code you  have to change \code{statefips} to \code{TRUE}. This variable is insensitive to case.
}
  \item{statefips}{
logical: are you providing \code{state} with a FIPS code instead of a name. By default, \code{statefips}=\code{FALSE}.
}
  \item{level}{
Takes in one of three values: "tract", "blk",  or "blkgrp". This defines the geographic level of data for the county.
}
  \item{bb.epsilon}{Takes in a numerical value. This value controls the number of blocks/block groups/tracts which are considered for clipping; if the function is not selecting some blocks/block groups/tracts then consider using a large value for \code{bb.epsilon}. Default \code{bb.epsilon} = 0.006.
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
%%\note{ }

\section{Warning }{You must have the packages \code{census2000blkgrp} and \code{census2000blk} installed to use levels "blkgrp" and "blk" respectively.} 


\seealso{
\code{\link{city}} \cr
% \code{\link[census2000cdp]{SpatialPolygonsDataFrame}}.
}
\examples{

##Perform the clipping
portland<-poly.clipper(name="Portland",state="OR",level="tract")

######Plot Portland as a Choropleth map

############################################
## Helper function for handling coloring of the map
############################################
color.map<- function(x,dem,y=NULL){
	l.poly<-length(x@polygons)
	dem.num<- cut(dem ,breaks=unique(ceiling(quantile(dem))),dig.lab = 6)
	dem.num[which(is.na(dem.num)==TRUE)]<-levels(dem.num)[1]
	l.uc<-length(table(dem.num))
if(is.null(y)){
	##commented out, but creates different color schemes 
	## using runif, may take a couple times to get a good color scheme.
	##col.heat<-rgb( runif(l.uc,0,1), runif(l.uc,0,1) , runif(l.uc,0,1) )
	col.heat<-heat.colors(16)[c(14,8,4,1)] ##fixed set of four colors
}else{
	col.heat<-y
	}
dem.col<-cbind(col.heat,names(table(dem.num)))
colors.dem<-vector(length=l.poly)
for(i in 1:l.uc){
	colors.dem[which(dem.num==dem.col[i,2])]<-dem.col[i,1]
	}
out<-list(colors=colors.dem,dem.cut=dem.col[,2],table.colors=dem.col[,1])
return(out)
}
############################################
## Helper function for handling coloring of the map
############################################

colors.use<-color.map(portland,as.numeric(portland@data$pop2000))
plot(portland,col=colors.use$colors)
#text(coordinates(alabama.blk),alabama.blk@data$name,cex=.3)
title(main="Census Tracts of\n Portland Oregon, 2000", sub="Quantiles (equal frequency)")
legend("bottomright",legend=colors.use$dem.cut,fill=colors.use$table.colors,bty="o",title="Population Count",bg="white")



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{CDP}
\keyword{demography}% __ONLY ONE__ keyword per line
\keyword{chapefile}
\keyword{polygon}
\keyword{clipping}

