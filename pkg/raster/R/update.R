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

	if (!fromDisk(object)) { 
		stop('object is not associated with a file on disk.')
	}

	cell <- na.omit(round(cell))
	stopifnot(length(cell) > 0)
	
	if (is.matrix(v)) {
		if (length(cell) > 1) {
			warning('only first cell used')
			cell <- cell[1] 
		}
		stopifnot(cell > 0)
		
		rc <- rowColFromCell(object, cell)
		if ((nrow(v) + rc[1] - 1) > nrow(object)) { 
			stop('attempting to update beyond end of file') 
		}
		if ((ncol(v) + rc[2] - 1) > ncol(object)) { 
			stop('attempting to update beyond end of file') 
		}
		dm <- dim(v)
		mat <- TRUE
	} else {
		stopifnot( is.vector(v) ) 
		if (length(cell) > 1) {
			stopifnot(length(cell) == length(v))
			stopifnot(max(cell) <= ncell(object))
			stopifnot(min(cell) > 0)
		} else {
			stopifnot(cell > 0)
			if ((length(v) + cell - 1) > ncell(object)) {
				stop('attempting to update beyond end of file') 
			}
		}
		mat <- FALSE
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
	if (mat) {
		dim(v) <- dm
	}
	
	setminmax <- FALSE
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
			if (length(cell) == 1) {
				oldv <- na.omit(.cellValues(object, cell:(cell+length(v)-1)))
			} else {
				oldv <- na.omit(.cellValues(object, cell))
			}
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
				if (newmin | newmax) {
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
		on.exit( GDAL.close(gdal) )

		dr <- getDriverName(getDriver(gdal))
		if (! dr %in% .gdalWriteFormats()[,1]) {
			stop('cannot update this file format (GDAL driver)')
		}
		
		if (is.matrix(v)) {

			startrow <- rowFromCell(object, cell) - 1
			startcol <- colFromCell(object, cell) - 1
			putRasterData(gdal, t(v), band=band, offset= c(startrow, startcol) )

		} else {
		
			if (length(cell) == 1) {
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
			} else {
				rows <- rowFromCell(object, cell) - 1
				cols <- colFromCell(object, cell) - 1
				for (i in 1:length(cell)) {
					putRasterData(gdal, v[i], band=band, offset=c(rows[i], cols[i]))
				} 
			}
			
		}

		if (setminmax) {	
			b <- new("GDALRasterBand", gdal, band)
			statistics <- c(object@data@min, object@data@max, NA, NA)
			try ( .Call("RGDAL_SetStatistics", b, as.double(statistics), PACKAGE = "rgdal"), silent=TRUE )
			#GDAL.close(b)
		}

		return(object)
	}	

	

	if (.isNativeDriver(driver)) {
	
		# need to support this too:
		stopifnot(object@file@toptobottom)
		
		minv <- object@data@min
		maxv <- object@data@max
			
		object <- writeStart(object, filename(object), update=TRUE, format=driver, datatype=datatype, overwrite=TRUE)
		
		if (dtype == "INT" | dtype == "LOG") { 
			v[is.na(v)] <- as.integer(object@file@nodatavalue)		
		} else { 
			v[] <- as.numeric(v) 
		}

		if (is.matrix(v)) {
			for (r in 1:nrow(v)) {
				pos <- (cell-1) * object@file@dsize
				seek(object@file@con, pos, rw='w')
				writeBin(v[r,], object@file@con, size=object@file@dsize )
				cell <- cell + object@ncols
			}
		
		} else {
			if (length(cell) == 1) {
				pos <- (cell-1) * object@file@dsize
				seek(object@file@con, pos, rw='w')
				writeBin(v, object@file@con, size=object@file@dsize )
			} else {
				for (i in 1:length(cell)) {
					pos <- (cell[i]-1) * object@file@dsize
					seek(object@file@con, pos, rw='w')
					writeBin(v[i], object@file@con, size=object@file@dsize )
				}
			}
		}
		
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

