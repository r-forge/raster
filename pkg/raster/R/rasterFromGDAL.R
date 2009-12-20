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
	xn <- round(xn, digits=9)

	xx <- xn + gdalinfo[["res.x"]] * nc
	xx <- round(xx, digits=9)
	
	yn <- gdalinfo[["ll.y"]]
	yn <- round(yn, digits=9)
	yx <- yn + gdalinfo[["res.y"]] * nr
	yx <- round(yx, digits=9)

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
	filename(x) <- filename
	
	
	datatype <- "FLT4S"
	try ( datatype <- .getRasterDType ( attr(gdalinfo, 'df')[1] ), silent=TRUE )
	
	dataType(x) <- datatype
	
	x@file@driver <- 'gdal' 
	x@data@min <- rep(Inf, nlayers(x))
	x@data@max <- rep(-Inf, nlayers(x))

	projection(x) <- attr(gdalinfo, "projection")
	
#oblique.x   0  #oblique.y   0 
	x@data@source <- 'disk'
	return(x)
}

