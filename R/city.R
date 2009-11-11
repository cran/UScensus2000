


city<-function(name=NULL,state=NULL,statefips=FALSE){
	

city.aux<-function(name=NULL,state=NULL,statefips=FALSE){
############Check state

data("countyfips",envir = parent.frame())
assign("temp",countyfips)
#rm(countyfips,envir = .GlobalEnv)
assign("countyfips",temp)
############Check state
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


############Check state

############load CDP file (if necessary)

if(paste(state,".cdp",sep="")%in%ls(envir=globalenv())){
	}else{
	data(list=paste(state,".cdp",sep=""),envir = parent.frame())
	assign("temp",get(paste(state,".cdp",sep="")))
#	rm(list=paste(state,".cdp",sep=""),envir = .GlobalEnv)
	assign(paste(state,".cdp",sep=""),temp)
}
############load CDP file
temp.cdp<-get(paste(state,".cdp",sep=""))


out<-temp.cdp[which(tolower(temp.cdp$name)%in%tolower(name)==TRUE),]
	}
out<-city.aux(name,state,statefips)
}