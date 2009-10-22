# raster package
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


# Not used
.writeRasterAssign <- function(x, ...) {
	name <- deparse(substitute(x))
	x <- writeRaster(x, ...)
	assign(name, x, envir=parent.frame())
	return(invisible())
}
