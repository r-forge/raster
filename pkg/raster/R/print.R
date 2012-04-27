# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3



setMethod ('print', 'Raster', 
	function(x, ...) {
		if (inherits(x, 'RasterStack')) {
			show(x)
		} else {
			if (x@file@driver == 'netcdf') {
				nc <- open.ncdf(x@file@name)
				print(nc)
				close.ncdf(nc)
			} else if (is.factor(x)) {
				cat('factor levels (value attributes)\n')
				f <- x@data@attributes[[1]]
				if (nrow(f) > 15) { 
					f <- f[1:15,]
				}
				print(f)
			# cat('levels      :' , paste(object@data@levels, collapse=', '), '\n')
			# cat('labels      :' , paste(object@data@labels, collapse=', '), '\n')
			} else callNextMethod(x, ...)
		}
	}
)



setMethod ('print' , 'Spatial', 
	function(x, ...) {
	
		cat('class       :' , class(x), '\n')
		isRaster <- hasData <- FALSE
		if (.hasSlot(x, 'data')) {
			hasData <- TRUE
		}
		
		if (inherits(x, 'SpatialPixels') | inherits(x, 'SpatialGrid')) {
			isRaster <- TRUE
			cr <- x@grid@cells.dim

			object <- brick(x)
			nl <- ifelse(hasData, ncol(x@data), 0)
			cat ('dimensions  : ', cr[2], ', ', cr[1], ', ', prod(cr), ', ', nl, '  (nrow, ncol, ncell, nlayers)\n', sep="" ) 
			#cat ('ncell       :' , ncell(object), '\n')
			cs <- x@grid@cellsize
			cat ('resolution  : ' , cs[1], ', ', cs[2], '  (x, y)\n', sep="")		
		} else {		
			cat('nfeatures   :' , length(row.names(x)), '\n')
		}
		
		e <- bbox(x)
		cat('extent      : ' , e[1], ', ', e[2], ', ', e[3], ', ', e[4], '  (xmin, xmax, ymin, ymax)\n', sep="")
		cat('coord. ref. :' , projection(x, TRUE), '\n')
		if (.hasSlot(x, 'data')) {
			if (!isRaster) {
				cat('data dims   : ', nrow(x), ', ', ncol(x), '  (nrow, ncol)\n', sep="" ) 
			}

# this is problematic with factors			
#			r <- apply(x@data, 2, range, na.rm=TRUE)
#			minv <- as.vector(r[1,])
#			maxv <- as.vector(r[2,])
#			if (length(minv) > 10) {
#				minv <- c(minv[1:10], '...')
#				maxv <- c(maxv[1:10], '...')
#			}
#			cat('min values  :', paste(minv, collapse=' '), '\n')
#			cat('max values  :', paste(maxv, collapse=' '), '\n')

			coln <- colnames(x@data)
			if (length(coln) > 10) {
				coln <- c(coln[1:10], '...')
			}
			cat('variables   :', paste(coln, collapse=', '), '\n')
		}
	}
)	
	
