# R code for reading raster (grid) data
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : June 2008
# Version 0.8
# Licence GPL v3


.rasterRead <- function(raster, rownr,  startcol=1, ncolumns=(ncol(raster)-startcol+1)) {
	rownr <- round(rownr)
	ncolums <- round(ncolumns)
	if (rownr == 0) { stop("rownr == 0. It should be between 1 and nrow(raster), or < 0 for all rows") }
	if (rownr > nrow(raster)) { stop("rownr too high") }
	if (startcol < 1) { stop("startcol < 1") }
	if (startcol > ncol(raster)) { stop("startcol > ncol(raster)") }
	if (ncolumns < 1) { stop("ncols should be > 1") }

	endcol <- startcol + ncolumns - 1
	if (endcol > ncol(raster)) {
		warning("ncolumns too high, truncated")
		endcol <- ncol(raster) 
		ncolumns <- ncol(raster) - startcol + 1  
	}
	
	if (dataSource(raster)=='ram') {
		result <- valuesRow(raster, rownr)[startcol:endcol]
	} else 	if (.driver(raster) == 'raster') {
#		if dataContent(raster=='all')
		
		rastergri <- .setFileExtensionValues(filename(raster))
		if (!file.exists( filename(raster))) { 
			stop(paste(filename(raster)," does not exist"))
		}
		con <- file(rastergri, "rb")
		
		dtype <- .shortDataType(raster@file@datanotation)
		if (dtype == "INT" | dtype == "LOG" ) { 
			dtype <- "integer"
		} else {
			dtype <- "numeric" 
		}
		dsize <- dataSize(raster@file@datanotation)
		dsign <- dataSigned(raster@file@datanotation)
		
		if (rownr > 0) {
			seek(con, ((rownr-1) * ncol(raster) + (startcol-1)) * dsize)
			result <- readBin(con, what=dtype, n=ncolumns, dsize, dsign, endian=raster@file@byteorder) }	
		else {	
			result <- readBin(con, what=dtype, n=ncell(raster), dsize, dsign, endian=raster@file@byteorder) 
		}
		close(con)
#		result[is.nan(result)] <- NA
		if (dtype == 'numeric') {
			result[result <=  (0.999 * .nodatavalue(raster)) ] <- NA 	
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
				reg <- c(nrow(raster), ncol(raster)) #	reg <- dim(raster@file@gdalhandle[[1]])
			}
			else {
				offs= c((rownr-1), (startcol-1)) 
				reg <- c(1, ncolumns)
			}
		}
		result <- getRasterData(raster@file@gdalhandle[[1]], offset=offs, region.dim=reg, band = raster@file@band)
		
#		if (!is.vector(result)) {  result <- as.vector(result) 	}
		
		# if  setNAvalue() has been used.....
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
	} else if (startcol==1 & ncolumns==(ncol(raster)-startcol+1)) {
		raster@data@indices <- c(cellFromRowCol(raster, rownr, startcol), cellFromRowCol(raster, rownr, endcol))
		raster@data@content <- "row"
	} else {
		raster@data@indices <- c(cellFromRowCol(raster, rownr, startcol), cellFromRowCol(raster, rownr, endcol))
		raster@data@content <- "block"
	}	
	
	return(raster)
}
