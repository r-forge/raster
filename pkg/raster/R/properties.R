# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  October 2008
# Version 0,7
# Licence GPL v3


filename <- function(object) {
	if (class(object) == 'RasterStack') { 
		return(object@filename) 
	} 
	return(object@file@name)
}

ncells <- function(object) {
	return(return( as.numeric(nrow(object)) * ncol(object )))
}

xmin <- function(object) {
	object <- getBbox(object)
	return(as.numeric(object@xmin))
}

xmax <- function(object) {
	object <- getBbox(object)
	return(as.numeric(object@xmax))
}

ymin <- function(object) {
	object <- getBbox(object)
	return(as.numeric( object@ymin))
}

ymax <- function(object) {
	object <- getBbox(object)
	return(as.numeric(object@ymax))
}

xres <- function(object) {
	return ( as.numeric( (xmax(object) - xmin(object)) / ncol(object))  )
}

yres <- function(object) {
	return (  as.numeric( (ymax(object) - ymin(object)) / nrow(object))  )
}

resolution <- function(object) {
	x <- xres(object)
	y <- yres(object)
	return(c(x, y))
}


nlayers <- function(object) {
	if (class(object) == "RasterLayer") {
		return(1)
	} else {
		return(object@data@nlayers)
	}	
}

layers <- function(object) {
	if (class(object) == "RasterLayer") {
		return(filename(object))
	} else 	if (class(object) == "RasterBrick") {
		return(paste(filename(object), "with", nlayers(object), "layers"))
	} else if (class(object) == "RasterStack") {
		l <- vector('character')
		for (i in 1:nlayers(object)) {
			l <- c(l, filename(asRasterLayer(object, i)))
		}
		return(l)
	}	
}


band <- function(object) {
	if (class(object) == "RasterBrick") {
		return(-1)
	} else {
		return(object@file@band)
	}	
}

nbands <- function(object) {
	if (class(object) == "RasterLayer") {
		return(1)
	} else {
		return(object@file@nbands)
	}	
}

projection <- function(object, asText=TRUE) {
	if (asText) {
		if (is.na(object@crs@projargs)) { 
			return("NA") 
		} else {
			return(object@crs@projargs)
		}	
	} else {
		return(object@crs)
	}
}



origin <- function(object) {
	x <- xmin(object) - xres(object)*(round(xmin(object) / xres(object)))
	y <- ymax(object) - yres(object)*(round(ymax(object) / yres(object)))
	return(c(x, y))
}


minValue <- function(object, layer=1) {
	if (layer < 1) { return(NA)
	} else return(object@data@min[layer])
}


maxValue <- function(object, layer=1) {
	layer <- round(layer)
	layer <- max(1, min(nlayers(object), layer))
	if (layer < 1) { return(NA)
	} else { return(object@data@max[layer]) }
}


dataContent <- function(object) {
	return(object@data@content)
}

dataIndices <- function(object) {
	return(object@data@indices)
}

dataSource <- function(object) {
	return(object@data@source)
}

.driver <- function(object) {
	return(object@file@driver)
}	

.nodatavalue <- function(object) {
	if (class(object) == 'RasterStack') {
		stop("no such thing exist for an entire 'RasterStack'")
	}
	return(object@file@nodatavalue)
}	
