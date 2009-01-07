# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  October 2008
# Version 0,7
# Licence GPL v3


filename <- function(object) {
	return(object@file@name)
}

ncells <- function(object) {
	return(return( as.numeric(nrow(object)) * ncol(object )))
}

xmin <- function(object) {
	return(as.numeric(object@bbox[1,1]))
}

xmax <- function(object) {
	return(as.numeric(object@bbox[1,2]))
}

ymin <- function(object) {
	return(as.numeric( object@bbox[2,1]) )
}

ymax <- function(object) {
	return(as.numeric(object@bbox[2,2]))
}

.zmin <- function(object) {
	return (object@bbox[3,1])
}

.zmax <- function(object) {
	return (object@bbox[3,2])
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

boundingbox <- function(object) {
	if (class(object) != "matrix") {
		b <- bbox(object)[1:2, 1:2]
	} else {
		b <- object[1:2, 1:2]
	}
	rownames(b) <- c("x", "y")
	colnames(b) <- c("min", "max")
	return(b)
}


nlayers <- function(object) {
	if (class(object) == "RasterLayer") {
		return(1)
	} else {
		return(object@data@nlayers)
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
		if (is.na(object@proj4string@projargs)) { 
			return("NA") 
		} else {
			return(object@proj4string@projargs)
		}	
	} else {
		return(object@proj4string)
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


compare <- function(rasters, origin=TRUE, resolution=TRUE, rowcol=TRUE, projection=TRUE, slack=0.01, stopiffalse=TRUE) {
	res <- TRUE
	if (length(rasters) < 2) {
		res <- F
		stop('length(rasters) < 2')
	}	
	res1 <- resolution(rasters[[1]])
	origin1 <- origin(rasters[[1]])
	for (i in 2:length(rasters)) { 
		if (rowcol) {
			if (ncol(rasters[[1]]) != ncol(rasters[[i]])) {
				res <- F
				if(stopiffalse) { stop('ncols different') } 
			}	
			if (nrow(rasters[[1]]) != nrow(rasters[[i]])) {
				res <- F
				if(stopiffalse) { stop('nrows different') }
			}
		}
		if (projection) {
			if (projection(rasters[[1]]) != projection(rasters[[2]]) )  { 
				res <- F
				if(stopiffalse) {stop('different projections')}
			}
		}
		resi <- resolution(rasters[[i]])
		xr <-  min(res1[1], resi[1])
		yr <-  min(res1[2], resi[2])
		if (resolution) {
			if (abs(resi[1] - res1[1]) > slack * xr) {
				res <- F
				if(stopiffalse)  { stop('different x resolution') }
			}	
			if (abs(resi[2] - res1[2]) > slack * yr) { 
				res <- F
				if(stopiffalse) { stop('different y resolution') }
			}
		}
		if (origin) {
			origini <- origin(rasters[[1]])
			if ((abs(origini[1] - origin1[1])) > slack * xr) {
				res <- F
				if(stopiffalse) { stop('different x origins') }
			} 
			if ((abs(origini[2] - origin1[2])) > slack * yr) {
				res <- F
				if(stopiffalse) { stop('different y origins')}
			}	
		}
	}
	return(res)
}


