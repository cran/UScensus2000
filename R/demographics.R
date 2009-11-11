demographics<-function (dem = "pop2000", state, statefips = FALSE, level = c("tract", 
    "blk", "blkgrp", "cdp", "msa", "county"), msaname = NULL) 
{
    demographics.aux <- function(dem = "pop2000", state, statefips = FALSE, 
        level = c("tract", "blk", "blkgrp", "cdp", "msa", "county"), 
        msaname = NULL) {
        state2 <- state
        state <- check.state(state, statefips)
        if (is.null(state)) {
            stop("Not a State! \n")
        }
       dem.fun <- function(dem, state, level) {
            require(paste("UScensus2000", level, sep = ""), character.only = TRUE)
            x <- paste(state, level, sep = ".")
            data(list = x, envir = parent.frame())
            temp <- get(x)
            out <- temp@data[,dem]
            out<-as.matrix(out)
            return(out)
        }
        if (level == "county") {
        	data(list=paste(state, "tract", sep = "."))
            temp <- get(paste(state, "tract", sep = "."))
            temp$state<-as.character(temp$state)
            temp$county<-as.character(temp$county)
            temp$tract<-as.character(temp$tract)
            tract.fips <-temp$tract
            tract.fips[nchar(tract.fips) == 4] <- paste(temp$tract[nchar(tract.fips) == 4], "00", sep = "")
            temp.names<-paste(temp$state,temp$county,tract.fips,sep = "")
			d<-which(duplicated(temp.names))
			temp<-temp[-d,]
            county.u <- unique(temp$county)
			out<-vector()
			
			for(i in 1:length(county.u)){
			index<-temp$county%in%county.u[i]
			tempD<-temp@data[index,dem]
			tempD<-sapply(tempD,
				function(x){
					if(is.character(x)) x[1]
					else sum(x)
					})
			out<-rbind(out,tempD)
			}
	data("countyfips",envir = parent.frame())
	assign("temp",countyfips)
	assign("countyfips",temp)
	out<-as.matrix(out)
	cfips<-substr(countyfips$fips[substr(countyfips$fips,3,5)%in%county.u & countyfips$statename%in%state],3,5)
	cname<-countyfips$countyname[substr(countyfips$fips,3,5)%in%county.u & countyfips$statename%in%state]
	m<-match(county.u,cfips)
	rownames(out)<-cname[m]
        }
        else if (level == "msa") {
            temp <- MSA(msaname = msaname, state = toupper(state2),level = "tract")
            temp$state<-as.character(temp$state)
            temp$county<-as.character(temp$county)
            temp$tract<-as.character(temp$tract)
            tract.fips <-temp$tract
            tract.fips[nchar(tract.fips) == 4] <- paste(temp$tract[nchar(tract.fips) == 4], "00", sep = "")
            temp.names<-paste(temp$state,temp$county,tract.fips,sep = "")
			d<-which(duplicated(temp.names))
			temp<-temp[-d,]
			county.u <- unique(temp$county)
			out<-vector()
			
			for(i in 1:length(county.u)){
			index<-temp$county%in%county.u[i]
			tempD<-temp@data[index,dem]
			tempD<-sapply(tempD,
				function(x){
					if(is.character(x)) x[1]
					else sum(x)
					})
			out<-rbind(out,tempD)			
			}
	data("countyfips",envir = parent.frame())
	assign("temp",countyfips)
	assign("countyfips",temp)
			out<-as.matrix(out)
			cfips<-substr(countyfips$fips[substr(countyfips$fips,3,5)%in%county.u & countyfips$statename%in%state],3,5)
			cname<-countyfips$countyname[substr(countyfips$fips,3,5)%in%county.u & countyfips$statename%in%state]
			m<-match(county.u,cfips)
			rownames(out)<-cname[m]
			
        }
        else if (level == "tract") {
            out <- dem.fun(dem, state, level)
            
            temp <- get(paste(state, level, sep = "."))
            tract.fips <- as.character(temp$tract)
            tract.fips[nchar(tract.fips) == 4] <- paste(as.character(temp$tract[nchar(tract.fips) == 
                4]), "00", sep = "") 
            temp.names<-paste(as.character(temp$state), as.character(temp$county), as.character(tract.fips),sep = "")
            d<-which(duplicated(temp.names))
            out<-out[-d,]       
            rownames(out) <-temp.names[-d]
        }
        else if (level == "blkgrp") {
            out <- dem.fun(dem, state, level)
            temp <- get(paste(state, level, sep = "."))
            tract.fips <- as.character(temp$tract)
            tract.fips[nchar(tract.fips) == 4] <- paste(temp$tract[nchar(tract.fips) == 4], "00", sep = "")
         fipState<-as.character(temp$state)   
         fipCounty<-as.character(temp$county)   
         fipBlkgrp<-as.character(temp$blkgrp)
            temp.names<-paste(fipState,fipCounty,tract.fips, fipBlkgrp, sep = "")
            d<-which(duplicated(temp.names))
            out<-out[-d,]       
            rownames(out) <-temp.names[-d] 
        }
        else if (level == "blk") {
            out <- dem.fun(dem, state, level)
            temp <- get(paste(state, level, sep = "."))
            temp.names<-as.character(temp$fips)
            d<-which(duplicated(temp.names))
            if(length(d)>0){out<-out[-d,]        
            rownames(out) <-temp.names[-d] 
            }else{
            	rownames(out) <-temp.names 
            	}
        }
        else if (level == "cdp") {
            out <- dem.fun(dem, state, level)
            temp <- get(paste(state, level, sep = "."))
            if (length(dem) == 1) {
                out <- out[match(unique(temp$name), temp$name)]
                names(out) <- unique(temp$name)
            }
            else {
                out <- out[match(unique(temp$name), temp$name), 
                  ]
                rownames(out) <- unique(temp$name)
            }
        }
        out
    }
    demographics.aux(dem, state, statefips, level, msaname)
}