# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  October 2008
# Version 0.8
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
	if (layer < 1) { 
		return(NA)
	} else {
		return(object@data@min[layer])
	}
}


maxValue <- function(object, layer=1) {
	layer <- round(layer)
	layer <- max(1, min(nlayers(object), layer))
	if (layer < 1) { return(NA)
	} else { return(object@data@max[layer]) }
}


.driver <- function(object) {
	if (class(object@file@con)[1] == 'file') {
		return('raster')
	} else if (class(object@file@con)[1] == "GDALReadOnlyDataset") {
		return('gdal')
	} else {
		stop('could not determine driver')
	}
}	

.nodatavalue <- function(object) {
	if (class(object) == 'RasterStack') {
		stop("no such thing exist for an entire 'RasterStack'")
	}
	return(object@file@nodatavalue)
}	
