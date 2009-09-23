# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date:  October 2008
# Version 0.8
# Licence GPL v3


layerNames <- function(object) {
	if (class(object) == "RasterLayer") {
		return(object@file@shortname)
	} else if (class(object) == "RasterStack") {
		return(object@layernames)
	} else if (class(object) == "RasterBrick") {
		return(object@data@colnames)	
	}
}


'layerNames<-' <- function(object, value) {
	if (length(value) != nlayers(object)) {
		stop('value has wrong length')
	}
	if (class(object) == "RasterLayer") {
		object@file@shortname <- value
		return(object)
	} else if (class(object) == "RasterBrick") {
		object@data@colnames <- value
		if (length(unique(object@data@colnames)) != nlayers(object)) {
			stop('layer names must be unique')
		}
		return(object)
	} else if (class(object) == "RasterStack") {
		object@layernames <- value
		if (length(unique(object@layernames)) != nlayers(object)) {
			stop('layer names must be unique')
		}
		return(object)
	}	
}
