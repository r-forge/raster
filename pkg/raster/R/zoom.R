# R function for the raster package
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2009
# Version 0.9
# Licence GPL v3


zoom <- function(x, extent=drawExtent(), maxpixels=100000, layer=1, new=TRUE, ...) {
	extent <- extent  # force to start with drawing
	if (new) { dev.new() }
	if (class(x) != 'RasterLayer') { x <- raster(x,layer) }
	.plotraster(x, maxpixels=maxpixels, extent=extent, ...) 	
}

