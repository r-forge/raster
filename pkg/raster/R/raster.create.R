# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : June 2008
# Version 0,1
# Licence GPL v3


newRaster <- function(xmin=-180, xmax=180, ymin=-90, ymax=90, nrows=180, ncols=360, projection="+proj=longlat +datum=WGS84") {
	bb <- newBbox(xmin, xmax, ymin, ymax, projection)
	return(rasterFromBbox(bb, nrows=nrows, ncols=ncols))
}

rasterFromBbox <- function(boundingbox, nrows=1, ncols=1) {
	nr = as.integer(round(nrows))
	nc = as.integer(round(ncols))
	if (nc < 1) { stop("ncols should be larger than 0") }
	if (nr < 1) { stop("nrows should be larger than 0") }
	if (validObject(boundingbox)) {
		raster <- new("RasterLayer", bbox = boundingbox@bbox, proj4string=boundingbox@proj4string, ncols = nc, nrows = nr )
		raster@data@content <- 'nodata'
		return(raster) 
	} else {
		return <- NA 
	}
}

rasterFromFile <- function(filename, values=FALSE, band=1) {
	fileext <- toupper(fileExtension(filename)) 
	if (fileext == ".GRD" ) {
		raster <- .rasterFromFile.binary(filename, band) 
	} else {
		raster <- .rasterFromFile.gdal(filename, band) 
	}
	if (values) {raster <- .raster.read(raster, -1)}
	return(raster)
}	
	
.rasterFromFile.gdal <- function(filename, band) {	
	gdalinfo <- GDALinfo(filename)
	nc <- as.integer(gdalinfo[["columns"]])
	nr <- as.integer(gdalinfo[["rows"]])
	xn <- gdalinfo[["ll.x"]]
	if (xn < 0) { ndecs <- 9 } else  { ndecs <- 8 }
	xn <- as.numeric( substr( as.character(xn), 1, ndecs) )

	xx <- xn + gdalinfo[["res.x"]] * nc
	if (xx < 0) { ndecs <- 9 } else  { ndecs <- 8 }
	xx <- as.numeric( substr( as.character(xx), 1, ndecs) )
		
#   as "ll" stands for lower left corner , it should be this,  yn <- gdalinfo[["ll.y"]];   yx <- ymin + gdalinfo[["res.y"]]  * raster$nrows
#  but in fact  the upper left corner "ul" is returned so we do this:
	yx <- gdalinfo[["ll.y"]]
	if (yx < 0) { ndecs <- 9 } else  { ndecs <- 8 }
	yx <- as.numeric( substr( as.character(yx), 1, ndecs) )

	yn <- yx - gdalinfo[["res.y"]] * nr
	if (yn < 0) { ndecs <- 9 } else { ndecs <- 8 }
	yn <- as.numeric( substr( as.character(yn), 1, ndecs) )
	raster <- newRaster(ncols=nc, nrows=nr, xmin=xn, ymin=yn, xmax=xx, ymax=yx, projection="")
	raster <- setFilename(raster, filename)
	raster <- setDatatype(raster, "numeric")
	

	raster@file@driver <- 'gdal' 
		#attr(gdalinfo, "driver")

	raster@file@nbands <- as.integer(gdalinfo[["bands"]])
	band <- as.integer(band)
	if (band > raster@file@nbands) {
		warning("band too high. Set to nbands")
		band <- raster@file@nbands }
	if ( band < 1) { 
		warning("band too low. Set to 1")
		band <- 1 }
	raster@file@band <- as.integer(band)

	raster <- setProjection(raster, attr(gdalinfo, "projection"))
	
	raster@file@gdalhandle[1] <- GDAL.open(filename)
#oblique.x   0  #oblique.y   0 
	raster@data@source <- 'disk'
	return(raster)
}



.rasterFromFile.binary <- function(filename, band=1) {
	ini <- readIniFile(filename)
	ini[,2] = toupper(ini[,2]) 

	byteorder <- .Platform$endian
	nbands <- as.integer(1)
	band <- as.integer(1)
	bandorder <- "BSQ"
	ncellvals <- -9
	projstring <- ""
	
	for (i in 1:length(ini[,1])) {
		if (ini[i,2] == "MINX") {xn <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "MAXX") {xx <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "MINY") {yn <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "MAXY") {yx <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "ROWS") {nr <- as.integer(ini[i,3])} 
		else if (ini[i,2] == "COLUMNS") {nc <- as.integer(ini[i,3])} 
		else if (ini[i,2] == "MINVALUE") {minval <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "MAXVALUE") {maxval <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "NODATAVALUE") {nodataval <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "DATATYPE") {inidatatype <- ini[i,3]} 
		else if (ini[i,2] == "BYTEORDER") {byteorder <- ini[i,3]} 
		else if (ini[i,2] == "NBANDS") {nbands <- ini[i,3]} 
		else if (ini[i,2] == "BANDORDER") {bandorder <- ini[i,3]} 
#		else if (ini[i,2] == "NCELLVALS") {ncellvals <- ini[i,3]} 
		else if (ini[i,2] == "PROJECTION") {projstring <- ini[i,3]} 
    }  

    raster <- newRaster(ncols=nc, nrows=nr, xmin=xn, ymin=yn, xmax=xx, ymax=yx, projection=projstring)
	raster <- setFilename(raster, filename)
	raster@file@driver <- "raster"

	raster@data@min <- minval
	raster@data@max <- maxval
	raster@data@haveminmax <- TRUE
	raster@file@nodatavalue <- nodataval
	
	inidatatype <- trim(inidatatype)
	if (substr(inidatatype, 1, 3) == "ForceIntOutput") { datatp="integer"
	} else { datatp="numeric" }
	datasz <- as.integer(substr(inidatatype, 4, 4))
	raster <- setDatatype(raster, datatype=datatp, datasize=datasz)
	if ((byteorder == "little") | (byteorder == "big")) { raster@file@byteorder <- byteorder } 	
	raster@file@nbands <- as.integer(nbands)
	raster@file@band <- as.integer(band)
	# check if   0 < band  <= nbands 
	raster@file@bandorder <- bandorder 
	# check if in ("BSQ", "BIP", "BIL")
#	raster@data@ncellvals <- as.integer(ncellvals)

	raster@data@source <- 'disk'
    return(raster)
}


