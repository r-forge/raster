# R code for reading raster (grid) data
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : June 2008
# Version 0.8
# Licence GPL v3


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
			result <- valuesRow(raster, rownr)[startcol:endcol]
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
		
		raster <- openConnection(raster)
		if (rownr > 0) {
			seek(raster@file@con, ((rownr-1) * ncol(raster) + (startcol-1)) * dsize)
			result <- readBin(raster@file@con, what=dtype, n=ncolumns, dsize, dsign, endian=raster@file@byteorder) }	
		else {	
			seek(raster@file@con, 0)
			result <- readBin(raster@file@con, what=dtype, n=ncell(raster), dsize, dsign, endian=raster@file@byteorder) 
		}
		raster <- closeConnection(raster)

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


