# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  October 2008
# Version 0.8
# Licence GPL v3



filename <- function(object) {
	if (class(object) == 'RasterStack') { 
		return(object@filename) 
	} 
	return(object@file@name)
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
	return(c(xres(object), yres(object)))
}



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

projection <- function(object, asText=TRUE) {
	if (extends(class(object), "BasicRaster")) { object <- object@crs 
	} else if (extends(class(object), "Spatial")) { object <- object@proj4string 
	} else if (class(object) == 'character') {return(object)
	} else if (class(object) != "CRS") { stop(paste('cannot use this object of class', class(object))) }
	
	if (asText) {
		if (is.na(object@projargs)) { 
			return("NA") 
		} else {
			return(trim(object@projargs))
		}	
	} else {
		return(object)
	}
}



origin <- function(object) {
	x <- xmin(object) - xres(object)*(round(xmin(object) / xres(object)))
	y <- ymax(object) - yres(object)*(round(ymax(object) / yres(object)))
	return(c(x, y))
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


dataContent <- function(object) {
	return(object@data@content)
}

dataIndices <- function(object) {
	return(object@data@indices)
}

dataSource <- function(object) {
	return(object@data@source)
}

dataType <- function(object) {
	return(object@file@datanotation)
}


dataSize <- function(object) {
	return(object@file@datasize)
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
