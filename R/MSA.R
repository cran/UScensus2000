
MSA <-
function(msafips=NULL,msaname=NULL,state=NULL,level=c("tract","blk","blkgrp")){
MSA.aux <-
function(msafips=NULL,msaname=NULL,state=NULL,level=c("tract","blk","blkgrp")){

	data("MSAfips",envir = parent.frame())
	assign("temp",MSAfips)
	#rm(MSAfips,envir = .GlobalEnv)
	assign("MSAfips",temp)
	data("MSAnames",envir = parent.frame())
	assign("temp",MSAnames)
	#rm(MSAnames,envir = .GlobalEnv)
	assign("MSAnames",temp)

msa.fips<-function(msafips,level){	
	m<-MSAfips$msa.cmsa.fips%in%msafips
	state<-MSAfips$fips.state[m==TRUE]
	fips<-MSAfips$fips.county[m==TRUE]

if(length(unique(state))==1){
	out<-county(fips=unique(fips),state=state[1],level=level)
	return(out)
}else{
	state<-state[match(unique(fips),fips)]
	fips<-unique(fips)
	state.u<-unique(state)
	
 out<-county(fips=fips[which(state==state.u[1])],state=state.u[1],level=level)
	for(i in 2:length(state.u)){
		out<-spRbind(out,county(fips=fips[which(state==state.u[i])],state=state.u[i],level=level))
		}
		return(out)
	}
}

if(!is.null(msafips)){
	out<-msa.fips(msafips,level)
	return(out)
	
}else if(!is.null(msaname)){
	if(!is.null(state)){
		msaname<-msaname
		link<-MSAnames$name
		msafips<-MSAnames$msa.cmsa.fips[which(regexpr(msaname,link)>0 & regexpr(state,link)>0)]
		out<-msa.fips(msafips,level)
		return(out)
		}else{
	
		msafips<-MSAnames$msa.cmsa.fip[MSAnames$name%in%msaname]
		out<-msa.fips(msafips,level)
		return(out)
	}
}

}
out<-MSA.aux(msafips,msaname,state,level)
}