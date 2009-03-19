# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : June 2008
# Version 0.8
# Licence GPL v3


.rasterFromGDAL <- function(filename, band) {	
	gdalinfo <- GDALinfo(filename)
	nc <- as.integer(gdalinfo[["columns"]])
	nr <- as.integer(gdalinfo[["rows"]])
	xn <- gdalinfo[["ll.x"]]
	if (xn < 0) { ndecs <- 9 } else  { ndecs <- 8 }
	xn <- as.numeric( substr( as.character(xn), 1, ndecs) )

	xx <- xn + gdalinfo[["res.x"]] * nc
	if (xx < 0) { ndecs <- 9 } else  { ndecs <- 8 }
	xx <- as.numeric( substr( as.character(xx), 1, ndecs) )
		
#	gdalv <- (packageDescription(pkg = "rgdal")$Version)
#	dif <- compareVersion(gdalv, "0.5-32")
#	if (dif < 0) {
#		yx <- gdalinfo[["ll.y"]]
#		yn <- yx - gdalinfo[["res.y"]] * nr
#	} else {
		yn <- gdalinfo[["ll.y"]]
		yx <- yn + gdalinfo[["res.y"]] * nr
#	}
		
	if (yn < 0) { ndecs <- 9 } else { ndecs <- 8 }
	yn <- as.numeric( substr( as.character(yn), 1, ndecs) )
	if (yx < 0) { ndecs <- 9 } else  { ndecs <- 8 }
	yx <- as.numeric( substr( as.character(yx), 1, ndecs) )

	raster <- raster(ncols=nc, nrows=nr, xmn=xn, ymn=yn, xmx=xx, ymx=yx, projs="")
	filename(raster) <- filename
	dataType(raster) <- "FLT4S"
	

#	raster@file@driver <- 'gdal' 
		#attr(gdalinfo, "driver")

	raster@file@nbands <- as.integer(gdalinfo[["bands"]])
	band <- as.integer(band)
	if (band > nbands(raster) ) {
		warning("band too high. Set to nbands")
		band <- nbands(raster) }
	if ( band < 1) { 
		warning("band too low. Set to 1")
		band <- 1 }
	raster@file@band <- as.integer(band)

	projection(raster) <- attr(gdalinfo, "projection")
	
	attr(raster@file, "con") <- GDAL.open(filename)
	
#oblique.x   0  #oblique.y   0 
	raster@data@source <- 'disk'
	return(raster)
}

