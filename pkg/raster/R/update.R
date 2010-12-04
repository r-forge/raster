# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : December 2010
# Version 0.9
# Licence GPL v3


.writeEmptyRaster <- function(x, filename, value, ...) {
	x <- writeStart(x, filename, ...)
	tr <- blockSize(x)
	v <- rep(NA, tr$size * x@ncols * nlayers(x))
	for (i in 1:tr$n) {
		if (i == tr$n) {
			v <- rep(NA, tr$nrows[i] * x@ncols * nlayers(x))
		}
		x <- writeValues(x, v)	
		x@data@min <- Inf
		x@data@max <- -Inf
	}
	return( writeStop(x) )
}

	
if (!isGeneric("update")) {
	setGeneric("update", function(object, ...)
		standardGeneric("update"))
}	

setMethod('update', signature(object='RasterLayer'), 
function(object, v, cell) {

	# should not happen:
	if (! is.vector(v)) { stop('v is not a vector') }
	cell <- round(cell[1])
	if (cell < 1) { stop("cell < 1") } 
	if ((length(v) + cell - 1) > ncell(object)) { stop('attempting to update beyond end of file') }

	if (!fromDisk(object)) { 
		cell <- cell:(cell+length(v)-1)
		object[cell] <- v
		return(object)
	}

	if (raster:::.driver(object) != 'raster') {	stop('you can only updated files (on disk) in the "raster" format') 	}
	
	datatype = dataType(object)
	object <- writeStart(object, filename(object), update=TRUE, format='raster', datatype=datatype, overwrite=TRUE)
	
	v[is.infinite(v)] <- NA
	rsd <- na.omit(v) # min and max values
	if (length(rsd) > 0) {
		object@data@min <- min(object@data@min, rsd)
		object@data@max <- max(object@data@max, rsd)
	}	
	if (object@file@dtype == "INT" ) { 
		v <- as.integer(round(v))  
		v[is.na(v)] <- as.integer(object@file@nodatavalue)		
	} else if ( object@file@dtype =='LOG' ) {
		v[v != 1] <- 0
		v <- as.integer(v)  
		v[is.na(v)] <- as.integer(object@file@nodatavalue)		
	} else { 
		v  <- as.numeric( v ) 
	}

	pos <- (cell-1) * object@file@dsize
	seek(object@file@con, pos, rw='w')
	writeBin(v, object@file@con, size=object@file@dsize )
	return( writeStop(object) )
} 
)

#library(raster)
#r <- raster(nrow=10, ncol=10)
#x <- writeEmptyRaster(r, filename='abc.grd', overwrite=TRUE, datatype='INT2S')
#x <- updateRaster(x, 1:8, cell=11)
#as.matrix(x)
