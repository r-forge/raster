# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2008
# Version 0.9
# Licence GPL v3


.rasterFromRasterFile <- function(filename, band=1, type='RasterLayer') {
	grifile <- .setFileExtensionValues(filename)
	if (!file.exists( grifile )){
		stop("no '.gri' file")
	}	
	ini <- readIniFile(filename)
	ini[,2] = toupper(ini[,2]) 

	byteorder <- .Platform$endian
	nbands <- as.integer(1)
	band <- as.integer(band)
	bandorder <- "BIL"
	ncellvals <- -9
	projstring <- ""
	minval <- NA
	maxval <- NA
	nodataval <- -Inf
	layernames <- ''
	
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
		
		else if (ini[i,2] == "MINVALUE") { try ( minval <-  as.numeric(unlist(strsplit(ini[i,3], ':')))[band], silent = TRUE ) }
		else if (ini[i,2] == "MAXVALUE") { try ( maxval <-  as.numeric(unlist(strsplit(ini[i,3], ':')))[band], silent = TRUE ) }
		else if (ini[i,2] == "NODATAVALUE") {nodataval <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "DATATYPE") {inidatatype <- ini[i,3]} 
		else if (ini[i,2] == "BYTEORDER") {byteorder <- ini[i,3]} 
		else if (ini[i,2] == "NBANDS") {nbands <- ini[i,3]} 
		else if (ini[i,2] == "BANDORDER") {bandorder <- ini[i,3]} 
#		else if (ini[i,2] == "NCELLVALS") {ncellvals <- ini[i,3]} 
		else if (ini[i,2] == "PROJECTION") {projstring <- ini[i,3]} 
		else if (ini[i,2] == "LAYERNAME") {layernames <- ini[i,3]} 
    }  
	if (projstring == 'GEOGRAPHIC') { projstring <- "+proj=longlat" }
	
	shortname <- basename(filename)
	ext(shortname) <- ""
	shortname <- gsub(" ", "_", shortname)

	if (type == 'RasterBrick') {
		x <- brick(ncols=nc, nrows=nr, xmn=xn, ymn=yn, xmx=xx, ymx=yx, projs=projstring)
		x@data@nlayers <-  as.integer(nbands)
		x@layernames <- rep("", nbands)
		ln <- unlist(strsplit(layernames, ':'))
		minl <- max(1, min(length(ln), nbands))
		for (i in 1:minl) {
			x@layernames[i] <- ln[[i]]
		}
		for (i in 1:nbands) {
			if (x@layernames[i] == "") {
				x@layernames[i] <- paste(shortname, '_', i, sep='')
			}
		}
		x@data@min <- minval
		x@data@max <- maxval
		x@file@band <- as.integer(-1)
	} else {
		x <- raster(ncols=nc, nrows=nr, xmn=xn, ymn=yn, xmx=xx, ymx=yx, projs=projstring)
		x@layernames <- strsplit(layernames, ':')[band]
		x@data@min <- minval[1]
		x@data@max <- maxval[1]
		x@file@band <- as.integer(band)
	}
	
	x@file@nbands <- as.integer(nbands)
	# check if   0 < band  <= nbands 

	if (bandorder %in% c("BSQ", "BIP", "BIL")) {
		x@file@bandorder <- bandorder 
	}

	x@file@name <- filename
	x@data@haveminmax <- TRUE
	x@file@nodatavalue <- nodataval

	#	raster@file@driver <- "raster"
	
	dataType(x) <- inidatatype
	
	if ((byteorder == "little") | (byteorder == "big")) { 
		x@file@byteorder <- byteorder 
	} 	
	x@data@source <- 'disk'

#	attr(raster@file, "con") <- file(grifile, "rb")

    return(x)
}
