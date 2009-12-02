# R function for the raster package
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2009
# Version 0.9
# Licence GPL v3


.zoom <- function(x, layer=1, maxpixels, extent=extent, ...) {
	if (class(x) != 'RasterLayer') {
		x <- raster(x,layer)
	}
	.plotraster(x, maxpixels=maxpixels, extent=extent, ...) 
}


	
zoom <- function(x, extent=drawExtent(), maxpixels=100000, new=TRUE, ...) {
	ext <- extent
#	xlim <- c(extent@xmin, extent@xmax)
#	ylim <- c(extent@ymin, extent@ymax)
	if (new) { dev.new() }
	# for reasons beyond me, this fails:
	# plot(x=x, xlim=xlim, ylim=ylim, ...)
	# so we have this workaround that will likely fail in some cases.
	.zoom(x=x, maxpixels=maxpixels, extent=extent, ...)
}
