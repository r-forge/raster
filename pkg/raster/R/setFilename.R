# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3


filename <- function(x) {
	if (class(x) == 'RasterStack') { 
		return(x@filename) 
	} 
	return(x@file@name)
}

'filename<-' <- function(x, value) {
	filename <- trim(value)
	if (is.na(filename) || is.null(filename)) {
		filename <- ""
	}
	filename <- path.expand(filename)
	if (class(x)=='RasterStack') {
		ext(filename) <- ".stk"
		x@filename <- filename
	} else {
		x@file@name <- filename
		shortname <- basename(filename)
		ext(shortname) <- ""
		shortname <- gsub(" ", "_", shortname)
		if (nbands(x) > 1) { 
			shortname <- paste(shortname, "_", band(x)) 
		} 
		x@file@shortname <- shortname
	}	
	return(x)	
}

