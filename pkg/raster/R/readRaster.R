# raster package
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2008
# Version 0.9
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
	
	if (dataSource(raster)=='ram') {
		if (rownr < 1) {
			if (dataContent(raster) != 'all') {
				stop('cannot read data for this RasterLayer')
			} 
			return(raster)
		} else {
			stop('there are no values associated with this RasterLayer')
		}
		
	} else if (.isNativeDriver( .driver(raster)) ) {
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
	} else if (.driver(raster) == 'ascii') {
	
		result <- .getAsciiData(raster, rownr, startcol, ncolumns)
		
	} else { #use GDAL  
		if (is.na(raster@data@band)) { result <- NA }
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
		result <- getRasterData(raster@file@con, offset=offs, region.dim=reg, band = raster@data@band)
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


