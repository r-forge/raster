# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  November 2008
# Version 0.9
# Licence GPL v3


writeFormats <- function() {
	if (require(rgdal)) { 
		gd <- gdalDrivers()
		gd <- as.matrix(subset(gd, gd[,3] == T))
		short <- c("raster", "ascii", "SAGA", "IDRISI", "BIL", as.vector(gd[,1]))
		long <- c("raster package format", "Arc ascii", "SAGA GIS", "IDRISI", "Band Interleaved by Line", as.vector(gd[,2]))
		m <- cbind(short, long)
	} else {
		short <- c("raster", "ascii", "SAGA", "IDRISI", "BIL", "")
		long <- c("raster package format", "Arc ascii", "SAGA GIS", "IDRISI", "Band Interleaved by Line", "rgdal not installed")
	}
	m <- cbind(short, long)
	colnames(m) <- c("name", "long_name")
	return(m)
}

 