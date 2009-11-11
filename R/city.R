


city<-function(name=NULL,state=NULL,statefips=FALSE,sp.object=NULL,proj=NULL){
	

city.aux<-function(name=NULL,state=NULL,statefips=FALSE,sp.object=NULL,proj=NULL){


############Check state
state<-check.state(state,statefips)

if(is.null(state)){
	cat("Not a State! \n")
	return()
	}
############Check state



############load CDP file (if necessary) or rename given sp-object
if(is.null(sp.object)==FALSE){
	
	####Check to make sure it was passed and sp-object
	if(class(sp.object)[1]!="SpatialPolygonsDataFrame"){
		cat("Not a SpatialPolygonsDataFrame object! \n")
		return()
		}
	assign(paste(state,".cdp",sep=""),sp.object)
	}else{
	data(list=paste(state,".cdp",sep=""),envir = parent.frame())
}
############load CDP file (if necessary) or rename given sp-object

temp.cdp<-get(paste(state,".cdp",sep=""))
temp<- which(tolower(temp.cdp$name)%in%tolower(name)==TRUE)
if(length(temp)==0){
	cat(name,"is not in this SpatialPolygonsDataFrame object \n or this city does not exist in this state!")
	return()
	}
	
out<-temp.cdp[temp,]

############ Check to see if we want to project this?
if(is.null(proj)==FALSE){
	require(rgdal)
	out<-spTransform(out,proj)
	}
############
out
	}
out<-city.aux(name,state,statefips,sp.object,proj)
}