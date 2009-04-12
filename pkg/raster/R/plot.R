# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3



setMethod("plot", signature(x='Raster', y='missing'), 
	function(x, y, ...)  {
		.plotraster(x, index=1, col=rev(terrain.colors(25)), subsample=TRUE, maxdim=500, addbox=TRUE, axes = TRUE, xlab="", ylab="", ...) 
	}
)	

setMethod("plot", signature(x='Raster', y='numeric'), 
	function(x, y=1, ...)  {
		.plotraster(x, index=y, col=rev(terrain.colors(25)), subsample=TRUE, maxdim=500, addbox=TRUE, axes = TRUE, xlab="", ylab="", ...) 
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
	

