# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  October 2008
# Version 0.9
# Licence GPL v3

band <- function(object) {
	if (class(object) == "RasterLayer") {
		return(object@file@band)
	} else {
		stop(paste("not implemented for:", class(object), "objects"))
	}
}

nbands <- function(object) {
	if (class(object) == "RasterLayer") {
		return(object@file@nbands)
	} else {
		stop(paste("not implemented for:", class(object), "objects"))
	}	
}



minValue <- function(object, layer=1) {
	layer <- round(layer)
	if (layer < 1) { 
		return(object@data@min)
	} else {
		return(object@data@min[layer])
	}
}


maxValue <- function(object, layer=1) {
	layer <- round(layer)
	if (layer < 1) { 
		return(object@data@max)
	} else { 
		return(object@data@max[layer]) 
	}
}


.driver <- function(object) {
	fn <- filename(object)
	if (fn == '') {
		stop('no file asociated with this object')
	}
	fileext <- toupper(ext(fn)) 
	if ( fileext == ".GRD" | fileext == ".GRI" ) {
		return('raster')
	} else {
		return('gdal')
	}
	
#	fcon <- class(try( object@file@con, silent = T ))[1]
#	if (fcon == 'file') {
#		return('raster')
#	} else if (fcon == "GDALReadOnlyDataset") {
#		return('gdal')
#	} else if (fcon == "try-error") {
#		return('NA')
#	} else {
#		stop('unknown driver')
#	}

}	

.nodatavalue <- function(object) {
	if (class(object) == 'RasterStack') {
		stop("no such thing exist for an entire 'RasterStack'")
	}
	return(object@file@nodatavalue)
}	
