
install.blk<-function(x){

if(x=="osx" | x=="linux"){install.packages("UScensus2000blk", repos="http://disasternets.calit2.uci.edu/census2000/R/",type="source")
	return()
	}

if(x=="windows"){
	stop("Not Available Yet")
	}

stop("x must equal linux, osx or windows")
}