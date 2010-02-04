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
			y <- 1:nlayers(x)
			if (length(y) > 16) {
				warning('only first 16 layers are plotted')
				y <- 1:16
			}
		} else {
			if (is.character(y)) {
				yy = NULL
				for (i in 1:length(y)) {
					yy = c(yy, which(layerNames(x) == y[i])[1])
				}
			}
			y = na.omit(y)
			y <- unique(as.integer(round(y)))
		}
		if (length(y) == 1) {
			.plotraster(raster(x, y), col=col, maxpixels=maxpixels, main=layerNames(x)[y], ...) 
		} else {

			nl <- length(y)
			nc <- ceiling(sqrt(nl))
			nr <- ceiling(nl / nc)
				
			old.par <- par(no.readonly = TRUE) 
			on.exit(par(old.par))
			par(mfrow=c(nr, nc))
			xa='n'
			rown=1
			coln=0
			for (i in 1:length(y)) {
				coln = coln + 1
				if (coln > nc) {
					coln <- 1
					rown = rown + 1
				}
				if (rown==nr) xa='s'
				if (coln==1) ya='s' else ya='n'
				.plotraster(raster(x, y[i]), col=col, maxpixels=maxpixels, xaxt=xa, yaxt=ya, main=layerNames(x)[y[i]], ...) 
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

