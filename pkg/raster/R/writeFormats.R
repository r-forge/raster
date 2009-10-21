# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  November 2008
# Version 0.9
# Licence GPL v3


.nativeDrivers <- function() {
	return(  c("raster", "SAGA", "IDRISI", "BIL", "surfer") )
}

.nativeDriversLong <- function() {
	return(  c("raster package format", "SAGA GIS", "IDRISI", "Band Interleaved by Line", "surfer" ) )
}


.isNativeDriver <- function(d) {
	return ( (d %in% .nativeDrivers() ) )
}

writeFormats <- function() {
	if (require(rgdal)) { 
		gd <- gdalDrivers()
		gd <- as.matrix(subset(gd, gd[,3] == T))
		short <- c(.nativeDrivers(), 'ascii',  as.vector(gd[,1]))
		long <- c(.nativeDriversLong(), 'Arc ASCII', as.vector(gd[,2]))
		m <- cbind(short, long)
	} else {
		short <- c(.nativeDrivers(), 'ascii', "")
		long <- c(.nativeDriversLong(), "Arc ASCII", "rgdal not installed")
	}
	m <- cbind(short, long)
	colnames(m) <- c("name", "long_name")
	return(m)
}

 