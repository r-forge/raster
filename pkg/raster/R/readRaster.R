# R code for reading raster (grid) data
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2008
# Version 0.9
# Licence GPL v3

.getRasterData <- function(raster, rownr, startcol, ncolumns, dtype, dsize, dsign, offset=0) {
	raster <- openConnection(raster)
	if (rownr > 0) {
		seek(raster@file@con, ((rownr-1) * ncol(raster) + (startcol-1) + offset) * dsize)
		result <- readBin(raster@file@con, what=dtype, n=ncolumns, dsize, dsign, endian=raster@file@byteorder) 
	} else {	
		seek(raster@file@con, offset)
		result <- readBin(raster@file@con, what=dtype, n=ncell(raster), dsize, dsign, endian=raster@file@byteorder)
	}
	raster <- closeConnection(raster)
	return(result)
}


.getBilRasterData <- function(raster, rownr, startcol, ncolumns, dtype, dsize, dsign, band=1) {
	raster <- openConnection(raster)
	if (rownr > 0) {
		seek(raster@file@con, ((rownr-1) * ncol(raster) * nbands(raster) + (startcol-1) + (band-1) * ncol(raster)) * dsize)
		result <- readBin(raster@file@con, what=dtype, n=ncolumns, dsize, dsign, endian=raster@file@byteorder)
	} else {	
		result <- vector(length=0)
		for (rownr in 1:nrow(raster)) {
			seek(raster@file@con, ((rownr-1) * ncol(raster) * nbands(raster) + (startcol-1) + (band-1) * ncol(raster)) * dsize)
			res <- readBin(raster@file@con, what=dtype, n=ncolumns, dsize, dsign, endian=raster@file@byteorder)
			result <- c(result, res)
		}
	}
	raster <- closeConnection(raster)
	return(result)
}


.getBipRasterData <- function(raster, rownr, startcol, ncolumns, dtype, dsize, dsign, band=1) {
	raster <- openConnection(raster)
	index <- rep(FALSE, nbands(raster))
	index[band] <- TRUE
	index <- rep(index, ncolumns)
	if (rownr > 0) {
		seek(raster@file@con, ((rownr-1) * ncol(raster) * nbands(raster) + (startcol-1) * nbands(raster)) * dsize)
		nc <- ncolumns * nbands(raster)
		result <- readBin(raster@file@con, what=dtype, n=nc, dsize, dsign, endian=raster@file@byteorder) 
		result <- result[index]
	} else {	
		result <- vector(length=0)
		for (rownr in 1:nrow(raster)) {
			seek(raster@file@con, ((rownr-1) * ncol(raster) * nbands(raster) + (startcol-1) * nbands(raster)) * dsize)
			nc <- ncolumns * nbands(raster)
			res <- readBin(raster@file@con, what=dtype, n=nc, dsize, dsign, endian=raster@file@byteorder) 
			res <- res[index]
			result <- c(result, res)
		}
	}
	raster <- closeConnection(raster)
	return(result)
}


.rasterRead <- function(raster, rownr, startcol=1, ncolumns=(ncol(raster)-startcol+1)) {

	rownr <- round(rownr)
	ncolums <- round(ncolumns)
	if (rownr == 0) { stop("rownr == 0. It should be between 1 and nrow(raster), or < 0 for all rows") }
	if (rownr > nrow(raster)) { stop("rownr too high") }
	if (startcol < 1) { stop("startcol < 1") }
	if (startcol > ncol(raster)) { stop("startcol > ncol(raster)") }
	if (ncolumns < 1) { stop("ncols should be > 1") }

	endcol <- startcol + ncolumns - 1
	
# allowing this for readRows
#	if (endcol > ncol(raster)) {
#		warning("ncolumns too high, truncated")
#		endcol <- ncol(raster) 
#		ncolumns <- ncol(raster) - startcol + 1  
#	}
	
	if (dataSource(raster)=='ram') {
		if (rownr < 1) {
			if (dataContent(raster) != 'all') {
				stop('cannot read data for this RasterLayer')
			} 
			return(raster)
		} else {
			stop('cannot read a row of data for a raster that only exists in memory. Use getValues(raster, rownr)')
		}
		
	} else if (.driver(raster) == 'raster') {
		
		rastergri <- .setFileExtensionValues(filename(raster))
		if (!file.exists( filename(raster))) { 
			stop(paste(filename(raster)," does not exist"))
		}
		
		dtype <- .shortDataType(raster@file@datanotation)
		if (dtype == "INT" | dtype == "LOG" ) { 
			dtype <- "integer"
		} else {
			dtype <- "numeric" 
		}
		dsize <- dataSize(raster@file@datanotation)
		dsign <- dataSigned(raster@file@datanotation)
		
		if (nbands(raster) > 1) {
			band <- band(raster)
			bo <- raster@file@bandorder
			if (bo == 'BSQ') {
				offs <- (band-1) * ncell(raster)
				result <- .getRasterData(raster=raster, rownr=rownr, startcol=startcol, ncolumns=ncolumns, dtype=dtype, dsize=dsize, dsign=dsign, offset=offs) 
			} else if (bo == 'BIL') {
				result <- .getBilRasterData(raster=raster, rownr=rownr, startcol=startcol, ncolumns=ncolumns, dtype=dtype, dsize=dsize, dsign=dsign, band=band) 
			} else if (bo == 'BIP') {
				result <- .getBipRasterData(raster=raster, rownr=rownr, startcol=startcol, ncolumns=ncolumns, dtype=dtype, dsize=dsize, dsign=dsign, band=band) 
			} 
		} else {
			result <- .getRasterData(raster=raster, rownr=rownr, startcol=startcol, ncolumns=ncolumns, dtype=dtype, dsize=dsize, dsign=dsign, offset=0) 
		}

#		result[is.nan(result)] <- NA
		if (dtype == 'numeric') {
			result[result <=  (0.999999 * .nodatavalue(raster)) ] <- NA 	
			result[is.nan(result)] <- NA
		} else {
			result[result == raster@file@nodatavalue ] <- NA 			
		}
		if (dtype == 'logical') {
			result <- as.logical(result)
		}
	}
	else { #use GDAL  
		if (is.na(raster@file@band)) { result <- NA }
		else {
			if (rownr <= 0) {
				offs <- c(0, 0) 
				reg <- c(nrow(raster), ncol(raster)) 
			}
			else {
				offs= c((rownr-1), (startcol-1)) 
				reg <- c(1, ncolumns)
			}
		}
		
		raster <- openConnection(raster)
		result <- getRasterData(raster@file@con, offset=offs, region.dim=reg, band = raster@file@band)
		raster <- closeConnection(raster)
	
		# if  NAvalue() has been used.....
		if (raster@file@nodatavalue < 0) {
			result[result <= raster@file@nodatavalue ] <- NA 			
		} else {
			result[result == raster@file@nodatavalue ] <- NA 					
		}	
	} 
	
	raster@data@values <- as.vector(result)
	if (rownr < 0) {
		raster@data@indices <- c(1, ncell(raster))
		raster@data@content <- "all"
		raster <- setMinMax(raster)
	} else {
		raster@data@content <- 'row' 
		firstcell <- cellFromRowCol(raster, rownr, 1)
		lastcell <- cellFromRowCol(raster, rownr, raster@ncols)
		raster@data@indices <- c(firstcell, lastcell)
	}	
	return(raster)
}


