# raster package
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


# Not used
.writeRasterAssign <- function(raster, filetype='raster', overwrite=FALSE) {
	name <- deparse(substitute(raster))
	raster <- writeRaster(raster, filetype=filetype, overwrite=overwrite)
	assign(name, raster, envir=parent.frame())
	return(invisible())
}
