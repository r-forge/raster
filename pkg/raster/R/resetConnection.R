# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : April  2009
# Version 0.8
# Licence GPL v3



openConnection <- function(raster) {
	if (class(raster) != 'RasterLayer') {
		stop('resetConnection is only for RasterLayer objects')
	}
	fn <- trim(filename(raster))
	if (fn == "") { stop('no file') }
	fileext <- toupper(ext(fn)) 
	if ( fileext == ".GRD" ) {
		ext(fn) <- '.gri'
		attr(raster@file, "con") <- file(fn, "rb")
	} else {
		attr(raster@file, "con") <- GDAL.open(fn)
	}
	return(raster)
}


