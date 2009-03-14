# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3



setFilename <- function(x, value) {
	filename <- value
	if (is.na(filename)) {filename <- ""}
	filename <- trim(filename)
	if (class(x)=='RasterStack') {
		x@filename <- setFileExtension(filename, ".stk")
	} else {
		x@file@name <- filename
	}	
	if (class(x)=='RasterLayer') {
		shortname <- shortFileName(filename)
		shortname <- setFileExtension(shortname, "")
		shortname <- gsub(" ", "_", shortname)
		if (nbands(x) > 1) { shortname <- paste(shortname, "_", band(x)) } 
		x@file@shortname <- shortname
		x@file@gdalhandle <- list()
	}	
	return(x)	
}

