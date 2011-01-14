# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  January 2009
# Version 0.9
# Licence GPL v3


setMethod("[", c("Raster", "Spatial", "missing"),
function(x, i, j, ... ,drop=TRUE) {
	if (inherits(i, 'SpatialGrid') | inherits(i, 'SpatialPixels')) {
		i <-  as(i, 'SpatialPoints')
	}
	if (drop) {
		extract(x, i, ...)
	} else {
		x <- crop(x, i, ...)
		rasterize(i, x, mask=TRUE, ...)
	}
})


setMethod("[", "Raster",
function(x, i, j, drop=TRUE) {
	
	if (! hasValues(x) ) {
		stop('no data associated with this RasterLayer object')
	}

	if (! missing(j) ) { 
		if (! is.numeric(j)) { 
			stop('the second argument must be numeric (or missing)') 
		}	
		if (! missing(i)) {
			if (! (is.numeric(i) | is.logical(i)) ) {
				stop('you cannot supply a second argument if the first is not numeric or logical') 		
			}
		}
	}
	
	if (missing(i)) {
		if (missing(j)) {
			if (drop) {
				return(getValues(x))
			} else {
				return(x)
			}
		} else {
			i <- cellFromCol(x, j)
		}
	} else {
		if (inherits(i, "RasterLayer")) {
			i <- (1:ncell(i))[ as.logical( getValues(i) ) ]
		} else if (inherits(i, "Extent")) {
			if (drop) {
				return( extract(x, i) )
			} else {
				return( crop(x, i) )
			}
		} else {
			if (missing(j)) {
				theCall <- sys.call(-1)
				narg <- length(theCall) - length(match.call(call=sys.call(-1)))
				if (narg > 0) {
					i <- cellFromRow(x, i)
				}
			} else {
				i <- cellFromRowColCombine(x, i, j)
			}
		}
	}
	nacount <- sum(is.na(i))
	if (nacount > 0) {
		warning('some indices are invalid (NA returned)')
	}	
	if (drop) {
		return( .cellValues(x, i) )
	} else {
		i <- na.omit(i)
		r <- rasterFromCells(x, i, values=FALSE)
		newi <- cellFromXY(r, xyFromCell(x, i))
		if (nlayers(x) > 1) {
			r <- brick(r)
			v <- matrix(NA, nrow=ncell(r), ncol=nlayers(x))
			v[newi,] <- .cellValues(x, i)
			v <- setValues(r, v)
			return(v)
		} else {
			r[newi] <- .cellValues(x, i)
			return(r)
		}
	}
}
)
