# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  April 2012
# Version 1.0
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



setMethod ('show' , 'Spatial', 
	function(object) {
		print (object)
	}
)


setMethod ('print' , 'Spatial', 
	function(x, ...) {
	
		cat('class       :' , class(x), '\n')
		isRaster <- hasData <- FALSE
		nc <- 0
		if (.hasSlot(x, 'data')) {
			hasData <- TRUE
			nc <- ncol(x@data)
		}
		
		if (inherits(x, 'SpatialPixels')) {
			isRaster <- TRUE
			cr <- x@grid@cells.dim
			cat ('dimensions  : ', cr[2], ', ', cr[1], ', ', nrow(x@coords), ', ', nc, '  (nrow, ncol, npixels, nlayers)\n', sep="" ) 
			cs <- x@grid@cellsize
			cat ('resolution  : ', cs[1], ', ', cs[2], '  (x, y)\n', sep="")		

		} else if (inherits(x, 'SpatialGrid')) {
			isRaster <- TRUE
			cr <- x@grid@cells.dim
			cat ('dimensions  : ', cr[2], ', ', cr[1], ', ', prod(cr), ', ', nc, '  (nrow, ncol, ncell, nlayers)\n', sep="" ) 
			cs <- x@grid@cellsize
			cat ('resolution  : ', cs[1], ', ', cs[2], '  (x, y)\n', sep="")		
			
		} else {		
			cat('nfeatures   :' , length(x), '\n')
		}
		
		e <- bbox(x)
		cat('extent      : ' , e[1], ', ', e[2], ', ', e[3], ', ', e[4], '  (xmin, xmax, ymin, ymax)\n', sep="")
		cat('coord. ref. :' , projection(x, TRUE), '\n')
		
		
		if (hasData) {
			x <- x@data
			
			maxnl <- 15
			
			if (! isRaster) {
				cat('nvariables  : ', nc, '\n', sep="" ) 
			}
			if (nc > maxnl) {
				x <- x[, 1:maxnl]
			}
			coln <- colnames(x)
			if (nc > maxnl) {
				coln <- c(coln[1:maxnl], '...')
				
			}
			cat('variables   :', paste(coln, collapse=', '), '\n')
			
			nfact <- sapply(1:ncol(x), function(i) is.numeric(x[,i]))
			if (sum(nfact) > 1) {
				r <- apply(x[,which(nfact)], 2, range, na.rm=TRUE)
				fc <- as.character(nfact)
				fc[! nfact] <- '(f)'
				maxv <- minv <- fc
				minv[nfact] <- as.vector(r[1, ])
				maxv[nfact] <- as.vector(r[2,])
				if (nc > maxnl) {
					minv <- c(minv, '...')
					maxv <- c(maxv, '...')
				}
				cat('min values  :', paste(minv, collapse=', '), '\n')
				cat('max values  :', paste(maxv, collapse=', '), '\n')
			} 
		}
	}
)	
	
