demographics <-
function(dem="pop2000",state,statefips=FALSE,level=c("tract","blk","blkgrp","cdp","msa","county"), msaname=NULL){

demographics.aux <-
function(dem="pop2000",state,statefips=FALSE,level=c("tract","blk","blkgrp","cdp","msa","county"), msaname=NULL){



state2<-state

############Check state
state<-check.state(state,statefips)

if(is.null(state)){
	cat("Not a State! \n")
	return()
	}
############Check state



### Grab demographics	
dem.fun<-function(dem,state,level){
	#if(is.null(sp.object)){
	require(paste("UScensus2000",level,sep=""),character.only=TRUE)
	x<-paste(state,level,sep=".")
	data(list=x,envir = parent.frame())
	temp<-get(x)
	#}else{
	#temp<-sp.object
	#	}
	out<-temp@data[,dem]
	return(out)
	}
### Grab demographics


if(level=="county"){
	out<-dem.fun(dem,state,"tract")
	temp<-get(paste(state,"tract",sep="."))
	out<-as.matrix(out,nr=length(temp$county),nc<-length(dem))
	county.u<-unique(temp$county)

	out2<-matrix(0,nc=length(dem),nr=length(county.u))
	for(j in 1:length(county.u)){
		for(i in 1:length(dem)){
			out2[j,i]<-sum(as.numeric(out[which(temp$county%in%county.u[j]==TRUE),i]))
		}
	}
	out<-out2
	rownames(out)<-county.u
	colnames(out)<-dem
	out<-data.frame(out,stringsAsFactors=FALSE)
}else if(level=="msa"){
		temp<-MSA(msaname=msaname,state=toupper(state2),level="tract")
		county.u<-unique(temp$county)
		out<-as.matrix(temp@data[,dem],nr=length(temp$county),nc<-length(dem))
		out2<-matrix(0,nc=length(dem),nr=length(county.u))
		for(j in 1:length(county.u)){
			for(i in 1:length(dem)){
				out2[j,i]<-sum(as.numeric(out[county.u[j]==temp$county,i]))
			}
		}
		out<-out2
		rownames(out)<-county.u
		colnames(out)<-dem
		out<-data.frame(out,stringsAsFactors=FALSE)
}else if(level=="tract"){

			out<-dem.fun(dem,state,level)
			temp<-get(paste(state,level,sep="."))
			tract.fips<-temp$tract
			tract.fips[nchar(tract.fips)==4]<-paste(temp$tract[nchar(tract.fips)==4],"00",sep="")
			rownames(out)<-paste(temp$state,temp$county,tract.fips,sep="")
}else if(level=="blkgrp"){

			out<-dem.fun(dem,state,level)
			temp<-get(paste(state,level,sep="."))
			tract.fips<-temp$tract
			tract.fips[nchar(tract.fips)==4]<-paste(temp$tract[nchar(tract.fips)==4],"00",sep="")
			
			
			if(length(dem)==1){
				out<-out[match(unique(paste(temp$state,temp$county,tract.fips,temp$blkgrp,sep="")),paste(temp$state,temp$county,tract.fips,temp$blkgrp,sep=""))]
				names(out)<-unique(paste(temp$state,temp$county,tract.fips,temp$blkgrp,sep=""))
				}else{
			out<-out[match(unique(paste(temp$state,temp$county,tract.fips,temp$blkgrp,sep="")),paste(temp$state,temp$county,tract.fips,temp$blkgrp,sep="")),]
			rownames(out)<-unique(paste(temp$state,temp$county,tract.fips,temp$blkgrp,sep=""))
			}
			
}else if(level=="blk"){

			out<-dem.fun(dem,state,level)
			temp<-get(paste(state,level,sep="."))
			if(length(dem)==1){
				out<-out[match(unique(temp$fips),temp$fips)]
				names(out)<-unique(temp$fips)
				}else{
			out<-out[match(unique(temp$fips),temp$fips),]
			rownames(out)<-unique(temp$fips)
			}



}else if(level=="cdp"){
			out<-dem.fun(dem,state,level)
			temp<-get(paste(state,level,sep="."))
			if(length(dem)==1){
				out<-out[match(unique(temp$name),temp$name)]
				names(out)<-unique(temp$name)
				}else{
			out<-out[match(unique(temp$name),temp$name),]
			rownames(out)<-unique(temp$name)
			}
			

}


out
}
out<-demographics.aux(dem, state,statefips,level, msaname)
}


#######County, simply do above and sum over the tracts
####### MSA, we need the name of the msa in question sum over tracts