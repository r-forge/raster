# raster package
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


.setFileExtensionValues <- function(fname, type='raster') {
	if (type == 'raster') {
		ext(fname) <- ".gri"
	} else if (type == 'SAGA') {
		ext(fname) <- ".sdat"
	} else if (type == 'IDRISI') {
		ext(fname) <- ".rst"
	} else if (type == 'BIL') {
		ext(fname) <- ".bil"
	} else {
		stop('unknown filetype')
	}
	return(fname)
}
 
.setFileExtensionHeader <- function(fname, type='raster') {
	if (type == 'raster') {
		ext(fname) <- ".grd"
	} else if (type == 'SAGA') {
		ext(fname) <- "sgrd"
	} else if (type == 'IDRISI') {
		ext(fname) <- ".rdc"
	} else if (type == 'BIL') {
		ext(fname) <- ".hdr"
	} else {
		stop('unknown filetype')
	}
	return(fname)
}
 