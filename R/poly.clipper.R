
poly.clipper <-function(name,state,statefips=FALSE,level=c("tract","blk","blkgrp"),bb.epsilon=.006){
poly.clipper.aux <-function(name,state,statefips=FALSE,level=c("tract","blk","blkgrp"),bb.epsilon=.006){


#name<-"portland"
#state<-"or"
#level<-"tract"
#bb.epsilon<-.006
#statefips<-FALSE

#######City
city<-city(name,state,statefips)
#######City

data("countyfips",envir = parent.frame())
assign("temp",countyfips)
#rm(countyfips,envir = .GlobalEnv)
assign("countyfips",temp)
#######Load baslevel
if(!statefips){
	if(nchar(state)==2){
		temp<-countyfips$statename[countyfips$acronym==tolower(state)][1]
		if(is.na(temp)){
			state<-countyfips$statename[substr(countyfips$fips,1,2)==state][1]
			}else{
				state<-temp
			}
		}
}
require(paste("UScensus2000",level,sep=""),character.only=TRUE)

if(paste(state,level,sep=".")%in%ls(envir=globalenv())){
	x<-paste(state,level,sep=".")
	}else{
	x<-paste(state,level,sep=".")
data(list=x,envir = parent.frame())
assign("temp",get(x))
#rm(list=x,envir = .GlobalEnv)
assign(x,temp)
}

assign("temp.level",get(x))
#######Load baslevel

#######converter function
as.Polygons.gpc.poly <- function(x, ID) {
        thisPolys <- lapply(get.pts(x), function(p) {
                Polygon(rbind(as.matrix(cbind(p$x,p$y)),c(p$x[1],p$y[1])),
                        hole=p$hole)
        })
        Polygons(thisPolys, ID)        
}
#######converter function




##############grabing bounding box of city +/- epsilon
temp1<-bbox(city)
temp1[1,1]<-temp1[1,1]-bb.epsilon
temp1[1,2]<-temp1[1,2]+bb.epsilon
temp1[2,1]<-temp1[2,1]-bb.epsilon
temp1[2,2]<-temp1[2,2]+bb.epsilon

temp<-matrix(c(temp1[1,1],temp1[2,1],
temp1[1,2],temp1[2,1],
temp1[1,2],temp1[2,2],
temp1[1,1],temp1[2,2]
),nc=2,nr=4,byrow=TRUE)
##############grabing bounding box of city


#########Use bounding box +/- epsilon to choose which blocks to look at
temp.gcp<-as(temp, "gpc.poly")
temp.pb<-as.Polygons.gpc.poly(temp.gcp,"temp.gcp1")
test10<-SpatialPolygons(list(temp.pb),proj4string=CRS("+proj=longlat +datum=NAD83"))

temp9<-overlay(SpatialPoints(coordinates(temp.level), proj4string=CRS("+proj=longlat +datum=NAD83")),test10)




#########Use bounding box +/- epsilon to choose which blocks to look at

##Build subset
m<-temp.level[which(is.na(temp9)==FALSE),]
##Build subset


##Grab Coords
plys<-slot(m,"polygons")
coords<-sapply(plys,function(x) {
  slot(slot(x, "Polygons")[[1]], "coords")
})
##Grab Coords

##build gpc.poly
gpc.poly.coords<-sapply(coords,as,"gpc.poly")
##build gpc.poly

##take the intersection

#### Need to do this for each polygon
#### collect into single list
city.gpc<-vector("list",length(city@polygons))

for(i in 1:length(city.gpc)){
city.gpc[[i]]<-as(city@polygons[[i]]@Polygons[[1]]@coords,"gpc.poly")
	}
#city.gpc<-as(city@polygons[[1]]@Polygons[[1]]@coords,"gpc.poly")

city.gpc2<-city.gpc[[1]]
if(length(city.gpc)>1){
for(i in 2:length(city.gpc)){
city.gpc2<-append.poly(city.gpc2,city.gpc[[i]])
}
}
int.man<-sapply(gpc.poly.coords,intersect,city.gpc2)


#if(length(int.man[[i]]@pts)>0)

###### Build list withonly intersection polygons and collect index of nonintersection

int.man2<-list()
intersect.index<-vector()

for(i in 1:length(int.man)){
	if(length(int.man[[i]]@pts)>0){
		#cat(i,":","\n")
		int.man2<-c(int.man2,list(int.man[[i]]))
		intersect.index<-c(intersect.index,i)
		}else{
			}
	}
###### Build list withonly intersection polygons and collect index of nonintersection	
########################### Build into SP polygon dataframe


city.blk<-vector("list",length(int.man2))
for(i in 1:length(int.man2)){
city.blk[[i]]<-as.Polygons.gpc.poly(int.man2[[i]],paste(city$name[1],i,sep=""))
}

ply.city.blk<-SpatialPolygons(city.blk,proj4string=CRS("+proj=longlat +datum=NAD83"))

########################### Build into SP polygon dataframe

####### Add population attributes
m2<-m[intersect.index,]


###########Rebuild datafiles

area1<-sapply(slot(ply.city.blk,"polygons"),slot, "area")
area2<-sapply(slot(m2,"polygons"),slot, "area")


names.of.cols<-c("pop2000",
"white","black","ameri.es","asian",
"hawn.pi", "other","mult.race","hispanic",
"not.hispanic.t","nh.white","nh.black","nh.ameri.es",   
"nh.asian","nh.hawn.pi","nh.other","hispanic.t",
"h.white", "h.black","h.american.es","h.asian",
"h.hawn.pi","h.other", "males","females",
"age.under5","age.5.17","age.18.21","age.22.29",
"age.30.39","age.40.49","age.50.64","age.65.up",
"med.age", "med.age.m","med.age.f","households",
"ave.hh.sz","hsehld.1.m","hsehld.1.f","marhh.chd",
"marhh.no.c","mhh.child","fhh.child","hh.units",
"hh.urban","hh.rural","hh.occupied","hh.vacant",
"hh.owner","hh.renter","hh.1person","hh.2person",
"hh.3person","hh.4person","hh.5person","hh.6person",
"hh.7person","hh.nh.white.1p","hh.nh.white.2p","hh.nh.white.3p",
"hh.nh.white.4p","hh.nh.white.5p","hh.nh.white.6p","hh.nh.white.7p",
"hh.hisp.1p","hh.hisp.2p","hh.hisp.3p","hh.hisp.4p",
"hh.hisp.5p","hh.hisp.6p","hh.hisp.7p","hh.black.1p",   
"hh.black.2p","hh.black.3p","hh.black.4p","hh.black.5p",   
"hh.black.6p","hh.black.7p","hh.asian.1p","hh.asian.2p",   
"hh.asian.3p","hh.asian.4p","hh.asian.5p","hh.asian.6p",   
"hh.asian.7p")

tempvalue<-as.numeric(m2@data[,names.of.cols[1]])
newvalue<-ceiling((area1/area2)*tempvalue)

for(i in 2:length(names.of.cols)){
tempvalue<-as.numeric(m2@data[,names.of.cols[i]])
newvalue<-cbind(newvalue,ceiling((area1/area2)*tempvalue))
}

if(level=="tract"){
	temp<-data.frame(cbind(m2$state,m2$county,m2$tract,newvalue),stringsAsFactors=FALSE)
}else if(level=="blkgrp"){
	temp<-data.frame(cbind(m2$state,m2$county,m2$tract,m2$blkgrp,newvalue),stringsAsFactors=FALSE)
	}else{
	temp<-data.frame(cbind(m2$fips,newvalue),stringsAsFactors=FALSE)
		}
###nameing
colnames(temp)<-names(m2)
rownames(temp)<-sapply(slot(ply.city.blk,"polygons"),slot,"ID")





###########Rebuild datafiles



ply.city.blk<-SpatialPolygonsDataFrame(ply.city.blk,data=temp)

#return(ply.city.blk)
####### Add population attributes
}
out<-poly.clipper.aux(name,state,statefips,level,bb.epsilon)
}