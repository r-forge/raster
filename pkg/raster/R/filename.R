# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


filename <- function(x) {
	if (class(x) == 'RasterStack') { 
		return(x@filename) 
	} 
	return(x@file@name)
}

'filename<-' <- function(x, value) {
	filename <- trim(value)
	if (is.na(filename) | is.null(filename) | !is.character(value)) {
		filename <- ""
	}
	filename <- path.expand(filename)
# could also throw in normalizePath(utils) 
	if (class(x)=='RasterStack') {
		ext(filename) <- ".stk"
		x@filename <- filename
	} else {
		x@file@name <- filename
		if (class(x)=='RasterLayer') {	
			shortname <- basename(filename)
			ext(shortname) <- ""
			shortname <- gsub(" ", "_", shortname)
			if (nbands(x) > 1) { 
				shortname <- paste(shortname, "_", band(x), sep="") 
			} 
			x@layernames <- shortname
		}
	}
	return(x)	
}


#.filename <- function(..., filename) {
#	if (missing(filename)) { 
#		return('')
#	} else {
#		return(trim(filename))
#	}
#}


#.writefilename <- function(raster, ...) {
#	filename <- .filename(...) 
#	if (filename == '') {
#		filename <- trim(filename(raster))
#	}
#	if (filename == '') {
#		stop('provide a filename')
#	} else {
#		return(filename)
#	}
#}
