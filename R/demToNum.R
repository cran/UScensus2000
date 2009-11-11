demToNum<-function(sp.object=NULL){
	### Check to make sure the object is sp-class
	if(class(sp.object)[1]!="SpatialPolygonsDataFrame")
		cat("Incompatible class!")
	
	b<-which(names(sp.object)=="pop2000")
	e<-ncol(sp.object@data)
	for(i in 4:89){
	sp.object@data[,i]<-as.numeric(sp.object@data[,i])
	}
	sp.object
	}