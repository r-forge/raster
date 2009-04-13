# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3


setMethod("plot", signature(x='RasterStack', y='ANY'), 
	function(x, y, ...)  {
		if (missing(y)) {
			nl <- nlayers(x)
			if (nl > 25) {
				warning('only first 25 layers are mapped')
				nl <- 25
			}
			nc <- ceiling(sqrt(nl))
			nr <- ceiling(nl / nc)
			par(mfrow=c(nr, nc))
			for (i in 1:nl) {
				.plotraster(x, index=i, col=rev(terrain.colors(25)), subsample=TRUE, maxdim=500, addbox=TRUE, axes = TRUE, xlab="", ylab="", ...) 
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
					.plotraster(x, index=y[i], col=rev(terrain.colors(25)), subsample=TRUE, maxdim=500, addbox=TRUE, axes = TRUE, xlab="", ylab="", ...) 
				}
			} else {
				.plotraster(x, index=y, col=rev(terrain.colors(25)), subsample=TRUE, maxdim=500, addbox=TRUE, axes = TRUE, xlab="", ylab="", ...) 
			}		
		}
	}
)	


setMethod("plot", signature(x='RasterLayer', y='missing'), 
	function(x, ...)  {
		.plotraster(x, col=rev(terrain.colors(25)), subsample=TRUE, maxdim=500, addbox=TRUE, axes = TRUE, xlab="", ylab="", ...) 
	}
)	


setMethod("plot", signature(x='RasterLayer', y='RasterLayer'), 
	function(x, y, maxdim=10000, cex=0.1, ...)  {
		comp <- compare(c(x, y), bb=TRUE, rowcol=TRUE, prj=FALSE, tolerance=0.0001, stopiffalse=TRUE) 
		nc <- ncell(x)
		x <- sampleSkip(x, maxdim=maxdim)
		y <- sampleSkip(y, maxdim=maxdim)
		if (length(x) < nc) {
			warning(paste('plot used a sample of ', round(100*length(x)/nc), "% of the cells", sep=""))
		}
		plot(x, y, cex=cex, ...)			
	}
)
	

