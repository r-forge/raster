# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric("plot")) {
	setGeneric("plot", function(x,y,...)
		standardGeneric("plot"))
}	


setMethod("plot", signature(x='RasterStackBrick', y='ANY'), 
	function(x, y, col=rev(terrain.colors(255)), maxpixels=100000, ...)  {
		if (missing(y)) {
			nl <- nlayers(x)
			if (nl > 12) {
				warning('only first 12 layers are plotted')
				nl <- 12
			}
			nc <- ceiling(sqrt(nl))
			nr <- ceiling(nl / nc)
			par(mfrow=c(nr, nc))
			for (i in 1:nl) {	
				.plotraster(raster(x, i), col=col, maxpixels=maxpixels, main=layerNames(x)[i],  ...) 
			}
		} else if (is.numeric(y)) {
			y <- unique(as.integer(round(y)))
			if (length(y) > 1) {
				nl <- length(y)
				nc <- ceiling(sqrt(nl))
				nr <- ceiling(nl / nc)
				par(mfrow=c(nr, nc))
				par(mfrow=c(nr, nc))
				for (i in 1:length(y)) {
					.plotraster(raster(x, y[i]), col=col, maxpixels=maxpixels, main=layerNames(x)[i], ...) 
				}
			} else {
				.plotraster(raster(x, y), col=col, maxpixels=maxpixels, main=layerNames(x)[y], ...) 
			}		
		}
	}
)	


setMethod("plot", signature(x='RasterLayer', y='missing'), 
	function(x, col=rev(terrain.colors(255)), maxpixels=100000, levelplot=FALSE, ...)  {
		if (levelplot) .levelplotraster(x, col=col, maxpixels=maxpixels, ...) 
		else .plotraster(x, col=col, maxpixels=maxpixels, ...) 
	}
)	


setMethod("plot", signature(x='RasterLayer', y='RasterLayer'), 
	function(x, y, maxpixels=100000, cex=0.1, ...)  {
		comp <- compare(c(x, y), bb=TRUE, rowcol=TRUE, prj=FALSE, tolerance=0.0001, stopiffalse=TRUE) 
		nc <- ncell(x)
		x <- sampleRegular(x, n=maxpixels)
		y <- sampleRegular(y, n=maxpixels)
		if (length(x) < nc) {
			warning(paste('plot used a sample of ', round(100*length(x)/nc), "% of the cells", sep=""))
		}
		plot(x, y, cex=cex, ...)			
	}
)

