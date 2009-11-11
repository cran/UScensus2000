county <-
function(fips=NULL,name=NULL,state,level=c("tract","blk","blkgrp"),statefips=FALSE){

county.aux <-
function(fips=NULL,name=NULL,state,level=c("tract","blk","blkgrp"),statefips=FALSE){
###Load table with all the necessary county and fips info
data("countyfips",envir = parent.frame())
assign("temp",countyfips)
#rm(countyfips,envir = .GlobalEnv)
assign("countyfips",temp)

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
	
	

build.county.shape<-function(fips,state){		
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





assign("temp",get(x))

if(level=="blk"){
	out<-temp[which(substr(temp$fips,1,3)%in%fips==TRUE),]
	}else{
out<-temp[which(temp$county%in%fips==TRUE),]
}




return(out)
}

######Case 1
if(!is.null(fips)){
	build.county.shape(fips,tolower(state))
	}else{
fips<-substr(countyfips$fips[countyfips$countyname%in%tolower(name) & countyfips$statename%in%tolower(state)],3,5)
	build.county.shape(fips,tolower(state))
}

}

out<-county.aux(fips,name,state,level,statefips)
}