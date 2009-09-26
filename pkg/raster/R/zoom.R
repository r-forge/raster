# R function for the raster package
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2009
# Version 0.9
# Licence GPL v3


.zoom <- function(x, col=rev(terrain.colors(255)), layer=1, subsample=TRUE, maxdim=500, addbox=TRUE, axes = TRUE, xlab="", ylab="", ...) {
	if (class(x) != 'RasterLayer') {
		x <- raster(x,layer)
	}
	.plotraster(x, col=col, subsample=subsample, maxdim=maxdim, addbox=addbox, axes=axes, xlab=xlab, ylab=ylab, ...) 
}


	
zoom <- function(x, extent=drawBox(), new=TRUE, ...) {

	xlim <- c(extent@xmin, extent@xmax)
	ylim <- c(extent@ymin, extent@ymax)
	if (new) { dev.new() }
	# for reasons beyond me, this fails:
	# plot(x=x, xlim=xlim, ylim=ylim, ...)
	# so we have this workaround that will likely fail in some cases.
	.zoom(x=x, xlim=xlim, ylim=ylim, ...)
}
