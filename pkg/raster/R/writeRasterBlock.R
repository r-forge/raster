 
.writeRasterRows <- function(raster, filename, ...) {

	if (dataIndices(raster)[1] == 1) { 
		raster <- .startRowWriting(raster, filename, ...)
 	} 

	raster@data@values[is.nan(raster@data@values)] <- NA
	raster@data@values[is.infinite(raster@data@values)] <- NA
	if (raster@file@dtype == "INT" || raster@file@dtype =='LOG' ) { 
		values <- as.integer(round(raster@data@values))  
		values[is.na(values)] <- as.integer(raster@file@nodatavalue)		
	} else { 
		values  <- as.numeric( raster@data@values ) 
	}
	
	rsd <- na.omit(raster@data@values) # min and max values
	if (length(rsd) > 0) {
		raster@data@min <- min(raster@data@min, min(rsd))
		raster@data@max <- max(raster@data@max, max(rsd))
	}	
	
	writeBin(values, raster@file@con, size = raster@file@dsize )
	
	if (dataIndices(raster)[2] >= ncell(raster)) {
		raster <- .stopRowWriting(raster, ...)
		if (dataIndices(raster)[2] > ncell(raster)) {
			warning(paste('You have written beyond the end of file. last cell:', dataIndices(raster)[2], '>', ncell(raster)))
		}
	}
	return(raster)	
}

