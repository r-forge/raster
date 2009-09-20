# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3


setMethod("plot", signature(x='RasterStackBrick', y='ANY'), 
	function(x, y, col=rev(terrain.colors(255)), subsample=TRUE, maxdim=500, addbox=TRUE, axes = TRUE, xlab="", ylab="", ...)  {
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
				.plotraster(x, index=i, col=col, subsample=subsample, maxdim=maxdim, addbox=addbox, axes=axes, xlab=xlab, ylab=ylab, ...) 
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
					.plotraster(x, index=y[i], col=col, subsample=subsample, maxdim=maxdim, addbox=addbox, axes=axes, xlab=xlab, ylab=ylab, ...) 
				}
			} else {
				.plotraster(x, index=y, col=col, subsample=subsample, maxdim=maxdim, addbox=addbox, axes=axes, xlab=xlab, ylab=ylab, ...) 
			}		
		}
	}
)	


setMethod("plot", signature(x='RasterLayer', y='missing'), 
	function(x, col=rev(terrain.colors(255)), subsample=TRUE, maxdim=500, addbox=TRUE, axes = TRUE, xlab="", ylab="", ...)  {
		.plotraster(x, col=col, subsample=subsample, maxdim=maxdim, addbox=addbox, axes=axes, xlab=xlab, ylab=ylab, ...) 
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

