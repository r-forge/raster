# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : June 2008
# Version 0,1
# Licence GPL v3


closeHandle <- function(raster) {
#	if handle = gdal then gdalclose the handle
	if (.driver(raster) == "gdal") {
		closeDataset(raster@file@gdalhandle[[1]])
		raster@file@gdalhandle[[1]]	 <- list()
	} else {
		cr <- try(close(raster@filecon), silent = T)
	}
	return(raster)
}


newRaster <- function(xmn=-180, xmx=180, ymn=-90, ymx=90, nrows=180, ncols=360, projstring="+proj=longlat +datum=WGS84") {
	bb <- newBbox(xmn, xmx, ymn, ymx)
	return(rasterFromBbox(bb, nrows=nrows, ncols=ncols, projstring))
}

rasterFromBbox <- function(bndbox, nrows=1, ncols=1, projstring="") {
	bndbox <- getBbox(bndbox)
	nr = as.integer(round(nrows))
	nc = as.integer(round(ncols))
	if (nc < 1) { stop("ncols should be > 0") }
	if (nr < 1) { stop("nrows should be > 0") }
	crs <- newCRS(projstring)
	raster <- new("RasterLayer", bbox = bndbox, crs=crs, ncols = nc, nrows = nr )
	return(raster) 
}

rasterFromFile <- function(filename, values=FALSE, band=1) {
	fileext <- toupper(fileExtension(filename)) 
	if (fileext == ".RASTER" | fileext == ".GRD") {
		raster <- .rasterFromFileBinary(filename, band) 
	} else {
		raster <- .rasterFromFileGDAL(filename, band) 
	}
	if (values) {
		raster <- readAll(raster)
	}
	return(raster)
}	
	
.rasterFromFileGDAL <- function(filename, band) {	
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

	raster <- newRaster(ncols=nc, nrows=nr, xmn=xn, ymn=yn, xmx=xx, ymx=yx, projstring="")
	raster <- setFilename(raster, filename)
	raster <- setDatatype(raster, "numeric")
	

	raster@file@driver <- 'gdal' 
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

	raster <- setProjection(raster, attr(gdalinfo, "projection"))
	
	raster@file@gdalhandle[1] <- GDAL.open(filename)
#oblique.x   0  #oblique.y   0 
	raster@data@source <- 'disk'
	return(raster)
}



.rasterFromFileBinary <- function(filename, band=1) {
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
		else if (ini[i,2] == "XMIN") {xn <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "XMAX") {xx <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "YMIN") {yn <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "YMAX") {yx <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "ROWS") {nr <- as.integer(ini[i,3])} 
		else if (ini[i,2] == "COLUMNS") {nc <- as.integer(ini[i,3])} 
		else if (ini[i,2] == "NROWS") {nr <- as.integer(ini[i,3])} 
		else if (ini[i,2] == "NCOLS") {nc <- as.integer(ini[i,3])} 
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

    raster <- newRaster(ncols=nc, nrows=nr, xmn=xn, ymn=yn, xmx=xx, ymx=yx, projstring=projstring)
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


