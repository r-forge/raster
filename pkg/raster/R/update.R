# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : December 2010
# Version 0.9
# Licence GPL v3

	
if (!isGeneric("update")) {
	setGeneric("update", function(object, ...)
		standardGeneric("update"))
}	

setMethod('update', signature(object='RasterLayer'), 
function(object, v, cell) {
	
	cell <- round(cell[1]) 
	stopifnot(cell > 0)
	stopifnot( is.vector(v) ) 
	if ((length(v) + cell - 1) > ncell(object)) { 
		stop('attempting to update beyond end of file') 
	}
	
	if (!fromDisk(object)) { 
		stop('object has no values on disk, nothing to update')
		cell <- cell:(cell+length(v)-1)
		object[cell] <- v
		return(object)
	}

	band <- band(object)
	
	driver <- object@file@driver

	datatype <- object@file@datanotation
	dtype <- substr(datatype, 1, 3)
	if (dtype == "INT" ) { 
		v <- as.integer(round(v)) 
	} else if ( dtype =='LOG' ) {
		v[v != 1] <- 0
		v <- as.integer(v)  
	}
	v[is.infinite(v)] <- NA

	
	if (object@data@haveminmax) {
		rsd <- na.omit(v) 
		newmin <- FALSE
		newmax <- FALSE
		if (length(rsd) > 0) {
			minv <- min(rsd)
			maxv <- max(rsd)
			if (minv < object@data@min) { 
				newmin <- TRUE
			}
			if (maxv > object@data@max) { 
				newmax <- TRUE
			}
		}
		if (newmin & newmax) {
			object@data@min <- minv 
			object@data@max <- maxv
			setminmax <- TRUE
		} else {
			oldv <- na.omit(.cellValues(object, cell:(cell+length(v)-1)))
			if (length(oldv) > 0) {
				oldmin <- min(oldv)
				oldmax <- max(oldv)
				if (oldmin > object@data@min) {
					lostmin <- FALSE
				} else {
					lostmin <- TRUE
				}
				if (oldmax < object@data@max) {
					lostmax <- FALSE
				} else {
					lostmax <- TRUE
				}
			} else {
				lostmin <- FALSE
				lostmax <- FALSE
			}
			
			if (! (lostmin | lostmax) ) {
				if (! (newmin | newmax) ) {
					setminmax <- FALSE
				} else {
					object@data@min <- min(object@data@min, minv)
					object@data@max <- max(object@data@max, maxv)
					setminmax <- TRUE
				}
			} else if ((lostmin & newmin) & (! lostmax)) {
				object@data@min <- min(object@data@min, minv)
				setminmax <- TRUE
			} else if ((lostmax & newmax) & (! lostmin)) {
				object@data@max <- max(object@data@max, maxv)
				setminmax <- TRUE
			} else {
				object@data@min <- Inf
				object@data@max <- -Inf
				object@data@haveminmax <- FALSE				
				setminmax <- TRUE
			}
		}
	}

	
	if (driver == 'gdal') {	
		gdal <- new("GDALDataset", filename(object))
		cell <- cell:(cell+length(v)-1)
		rows <- rowFromCell(object, cell) - 1
		cols <- colFromCell(object, cell) - 1
		rows <- unique(rows)
		cols <- unique(cols)
		nr <- length(rows)
		if (nr == 1) {
			putRasterData(gdal, v, band=band, offset=c(rows, cols[1]))
		} else {
			offset <- c(rows[1], cols[1])
			nc <- object@ncols - cols[1]
			putRasterData(gdal, v[1:nc], band=band, offset=offset)
			v <- v[-(1:nc)]
			if (nr > 2) {
				nrows <- nr-2
				n <- nrows * object@ncols
				putRasterData(gdal, v[1:n], band=band, offset=c(rows[2], 0))
				v <- v[-(1:n)]
			}
			putRasterData(gdal, v, band=band, offset=c(rows[nr], 0))
		}
		
		if (setminmax) {	
			b <- new("GDALRasterBand", gdal, band)
			statistics <- c(object@data@min, object@data@max, NA, NA)
			try ( .Call("RGDAL_SetStatistics", b, as.double(statistics), PACKAGE = "rgdal"), silent=TRUE )
			#GDAL.close(b)
		}

		GDAL.close(gdal)
		return(object)
	}	

	if (.isNativeDriver(driver)) {
		minv <- object@data@min
		maxv <- object@data@max
			
		object <- writeStart(object, filename(object), update=TRUE, format='raster', datatype=datatype, overwrite=TRUE)
		if (dtype == "INT" | dtype == "LOG") { 
			v[is.na(v)] <- as.integer(object@file@nodatavalue)		
		} else { 
			v  <- as.numeric(v) 
		}

		pos <- (cell-1) * object@file@dsize
		seek(object@file@con, pos, rw='w')
		writeBin(v, object@file@con, size=object@file@dsize )

		object@data@min <- minv
		object@data@max <- maxv
		object@data@haveminmax <- TRUE
		object <- writeStop(object) 
		if (object@data@min == Inf) {
			object@data@haveminmax <- FALSE
			if (ncell(object) <= 1000000) {
				object <- setMinMax(object)
				hdr(object, driver)
			}
		}
		return( object )
	}

	if (driver == 'ncdf') {	
		stop('not yet implemented for ncdf files')
	}
	
	stop('not implemented for:  ', driver, '  files')
}	
)

