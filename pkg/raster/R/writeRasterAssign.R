
.writeRasterAssign <- function(raster, filetype='raster', overwrite=FALSE) {
	name <- deparse(substitute(raster))
	raster <- writeRaster(raster, filetype=filetype, overwrite=overwrite)
	assign(name, raster, envir=parent.frame())
	return(invisible())
}
