# R function for the raster package
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2009
# Version 0.9
# Licence GPL v3



zoom <- function(x, extent=drawBox(), new=TRUE, ...) {
	xlim <- c(extent@xmin, extent@xmax)
	ylim <- c(extent@ymin, extent@ymax)
	if (new) { dev.new() }
	plot(x, xlim=xlim, ylim=ylim, ...) 
}
