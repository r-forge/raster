# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3


.bboxmatrix <- function(x) {
	xy <- matrix(NA, nrow=5, ncol=2)
	xy[1,1] <- x@xmin
	xy[1,2] <- x@ymax
	xy[2,1] <- x@xmax
	xy[2,2] <- x@ymax
	xy[3,1] <- x@xmax
	xy[3,2] <- x@ymin
	xy[4,1] <- x@xmin
	xy[4,2] <- x@ymin
	return(xy)
}


setMethod("plot", signature(x='BoundingBox', y='ANY'), 
	function(x, y, ...)  {
		xy <- .bboxmatrix(x)
		xy[5,] <- xy[1,]
		plot(xy, type='l', ...)
		if (class(y) == 'BoundingBox') {
			lines(y)
		}
	}
)	


setMethod("plot", signature(x='Raster', y='missing'), 
	function(x, y, ...)  {
		map(x, ...)
	}
)	

setMethod("plot", signature(x='Raster', y='numeric'), 
	function(x, y, ...)  {
		map(x, y, ...)
	}
)		


setMethod("plot", signature(x='RasterLayer', y='RasterLayer'), 
	function(x, y, maxdim=1000, cex=0.1, ...)  {
		comp <- compare(c(x, y), bb=TRUE, rowcol=TRUE, prj=FALSE, tolerance=0.0001, stopiffalse=TRUE) 
		nc <- ncell(x)
		x <- readSkip(x, maxdim=maxdim)
		y <- readSkip(y, maxdim=maxdim)
		if (length(x) < nc) {
			warning(paste('plot used a sample of ', round(100*length(x)/nc), "% of the cells", sep=""))
		}
		plot(x, y, ...)			
	}
)
	

setMethod('hist', signature(x='Raster'), 
	function(x, layer=1, maxsamp=10000, ...){
		if (dataContent(x) != 'all') {
			if (dataSource(x) == 'disk') {
		# TO DO: make a function that does this by block and combines  all data into a single histogram

				x <- asRasterLayer(x, layer)
				if (ncell(x) <= maxsamp) {
					values <- na.omit(values(readAll(x)))
				} else {
					values <- readRandom(x, maxsamp)
					msg <- paste(round(100 * maxsamp / ncell(x)), "% of the raster cells were used", sep="")
					if (maxsamp > length(values)) {
						msg <- paste(msg, " (of which ", 100 - round(100 * length(values) / maxsamp ), "% were NA)", sep="")
					}
					msg <- paste(msg, ". ",length(values)," values used.", sep="")
					warning(msg)
				}	
			} else { stop('cannot make a histogram; need data on disk or in memory')}
		} else {
			values <- values(x)
		}			
		hist(values, ...)
	}	
)

