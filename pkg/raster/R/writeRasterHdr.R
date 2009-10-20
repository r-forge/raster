# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3

 
writeRasterHdr <- function(raster, filetype) {
	type <- toupper(filetype)
	if (type=="RASTER") {
		.writeHdrRaster(raster)
	} else if (type=="BIL") {
		.writeHdrBIL(raster)
		.writeStx(raster)
	} else if (type=="ERDASRAW") {
		.writeHdrErdasRaw(raster)
		.writeStx(raster)
	} else 	if (type=="ENVI") {
		.writeHdrENVI(raster)
		.writeStx(raster)
	} else 	if (type=="SAGA") {
		.writeHdrSAGA(raster)
	} else 	if (type=="IDRISI") {
		.writeHdrIDRISI(raster)
	} else {
		stop("This format is not supported")
	}
 }

 
 
.writeStx <- function(raster) {
	if (raster@data@haveminmax) {
		stxfile <- filename(raster)
		ext(stxfile) <- ".stx"
		thefile <- file(stxfile, "w")  # open an txt file connectionis
		cat(1, " ", minValue(raster), " ", maxValue(raster), "\n", file = thefile)
		close(thefile)
	}	
}
 