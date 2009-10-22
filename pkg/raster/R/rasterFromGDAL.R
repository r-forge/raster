# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2008
# Version 0.9
# Licence GPL v3


.rasterFromGDAL <- function(filename, band, type) {	
	if (!require(rgdal)) { stop() }

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

	if (type == 'RasterBrick') {
		x <- brick(ncols=nc, nrows=nr, xmn=xn, ymn=yn, xmx=xx, ymx=yx, projs="")
		x@data@nlayers <- as.integer(gdalinfo[["bands"]])
	} else {
		x <- raster(ncols=nc, nrows=nr, xmn=xn, ymn=yn, xmx=xx, ymx=yx, projs="")
		x@file@nbands <- as.integer(gdalinfo[["bands"]])
		band <- as.integer(band)
		if ( band > nbands(x) ) {
			warning("band too high. Set to nbands")
			band <- nbands(x) 
		}
		if ( band < 1) { 
			warning("band too low. Set to 1")
			band <- 1 
		}
		x@data@band <- as.integer(band)
	}

	shortname <- gsub(" ", "_", ext(basename(filename), ""))
	x <- .enforceGoodLayerNames(x, shortname)
	.setFilename(x) <- filename
	.setDataType(x) <- "FLT4S"
	
	x@file@driver <- 'gdal' 

	x@data@min <- rep(Inf, nlayers(x))
	x@data@max <- rep(-Inf, nlayers(x))

	projection(x) <- attr(gdalinfo, "projection")
	
#oblique.x   0  #oblique.y   0 
	x@data@source <- 'disk'
	return(x)
}

