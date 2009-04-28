# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : June 2008
# Version 0.8
# Licence GPL v3




.rasterFromRasterFile <- function(filename, band=1) {
	grifile <- .setFileExtensionValues(filename)
	if (!file.exists( grifile )){
		stop("no '.gri' file")
	}	
	ini <- readIniFile(filename)
	ini[,2] = toupper(ini[,2]) 

	byteorder <- .Platform$endian
	nbands <- as.integer(1)
	band <- as.integer(1)
	bandorder <- "BSQ"
	ncellvals <- -9
	projstring <- ""
	minval <- NA
	maxval <- NA
	nodataval <- -Inf
	
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
	if (projstring == 'GEOGRAPHIC') { projstring <- "+proj=longlat" }
    raster <- raster(ncols=nc, nrows=nr, xmn=xn, ymn=yn, xmx=xx, ymx=yx, projs=projstring)
	filename(raster) <- filename
#	raster@file@driver <- "raster"

	raster@data@min <- minval
	raster@data@max <- maxval
	raster@data@haveminmax <- TRUE
	raster@file@nodatavalue <- nodataval
	
	dataType(raster) <- inidatatype

	if ((byteorder == "little") | (byteorder == "big")) { raster@file@byteorder <- byteorder } 	
	raster@file@nbands <- as.integer(nbands)
	raster@file@band <- as.integer(band)
	# check if   0 < band  <= nbands 
	raster@file@bandorder <- bandorder 
	# check if in ("BSQ", "BIP", "BIL")
#	raster@data@ncellvals <- as.integer(ncellvals)

	raster@data@source <- 'disk'

#	attr(raster@file, "con") <- file(grifile, "rb")

    return(raster)
}


