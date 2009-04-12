# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  November 2008
# Version 0.8
# Licence GPL v3


writeFormats <- function() {
	gd <- gdalDrivers()
	gd <- as.matrix(subset(gd, gd[,3] == T))
	short <- c("raster", "ascii", as.vector(gd[,1]))
	long <- c("raster package format", "Arc ascii", as.vector(gd[,2]))
	m <- cbind(short, long)
	colnames(m) <- c("name", "long_name")
	return(m)
}

 