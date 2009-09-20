# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


.writeRasterHdr <- function(raster) {
	rastergrd <- .setFileExtensionHeader(filename(raster))
	thefile <- file(rastergrd, "w")  # open an txt file connectionis
	cat("[general]", "\n", file = thefile)
	cat("creator=R package:raster", "\n", file = thefile)
	cat("created=", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", file = thefile)
	cat("title=", raster@file@shortname, "\n", file = thefile)
	cat("[georeference]", "\n", file = thefile)
	cat("nrows=",  nrow(raster), "\n", file = thefile)
	cat("ncols=",  ncol(raster), "\n", file = thefile)
	cat("xmin=", xmin(raster), "\n", file = thefile)
	cat("ymin=", ymin(raster), "\n", file = thefile)
	cat("xmax=", xmax(raster), "\n", file = thefile)
	cat("ymax=", ymax(raster), "\n", file = thefile)
	cat("xres=", xres(raster), "\n", file = thefile)
	cat("yres=", yres(raster), "\n", file = thefile)
	cat("projection=", projection(raster), "\n", file = thefile)
	cat("[data]", "\n", file = thefile)
	cat("datatype=",  raster@file@datanotation, "\n", file = thefile)
	cat("byteorder=",  .Platform$endian, "\n", file = thefile)
	cat("nbands=",  nbands(raster), "\n", file = thefile)
	cat("bandorder=",  raster@file@bandorder, "\n", file = thefile)
	cat("minvalue=",  paste(minValue(raster,-1), collapse=':'), "\n", file = thefile)
	cat("maxvalue=",  paste(maxValue(raster,-1), collapse=':'), "\n", file = thefile)
	cat("nodatavalue=", .nodatavalue(raster), "\n", file = thefile)
#	cat("Sparse=", raster@sparse, "\n", file = thefile)
#	cat("nCellvals=", raster@data@ncellvals, "\n", file = thefile)	
	close(thefile)
}

