# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3

 
 .startRowWriting <- function(raster, overwrite) {
	filename(raster) <- trim(filename(raster))
	if (filename(raster) == "") {
		stop('first provide a filename. E.g.: raster <- setFilename(raster, "c:/myfile")')
	}
	
	raster <- setFilename(raster, .setFileExtensionHeader(filename(raster)))
	
	if (!overwrite & file.exists(filename(raster))) {
		stop(paste(filename(raster),"exists.","use 'overwrite=TRUE' if you want to overwrite it")) 
	}
	raster@file@name <- .setFileExtensionHeader(filename(raster))
	binraster <- .setFileExtensionValues(filename(raster))
	
	attr(raster@file, "con") <- file(binraster, "wb")
	attr(raster@file, "dsize") <- dataSize(raster@file@datanotation)
	attr(raster@file, "dtype") <- .shortDataType(raster@file@datanotation)
	
	raster@data@min <- Inf
	raster@data@max <- -Inf
	raster@data@haveminmax <- FALSE
	raster@file@driver <- 'raster'
	raster@file@gdalhandle <- list()
	return(raster)
}

.stopRowWriting <- function(raster) {
	.writeRasterHdr(raster) 
	close(raster@file@con)
	raster@data@haveminmax <- TRUE
	raster@data@source <- 'disk'
	raster@data@content <- 'nodata'
	raster@data@values <- vector(length=0)
	return(raster)
}		
 
 
.writeRasterRow <- function(raster, overwrite=FALSE) {
#	if (dataContent(raster) != 'row') { 
#		stop('raster does not contain a row') 
#	}

	if (dataIndices(raster)[1] == 1) { 
		raster <- .startRowWriting(raster, overwrite=overwrite)
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
		raster <- .stopRowWriting(raster)
		if (dataIndices(raster)[2] > ncell(raster)) {
			warning(paste('You have written beyond the end of file. last cell:', dataIndices(raster)[2], '>', ncell(raster)))
		}
	}
	return(raster)	
}

