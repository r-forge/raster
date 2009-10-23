# R function for the raster package
# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.9
# Licence GPL v3



.writeGDALrow <- function(raster, filename, mvFlag=NA, options=NULL, ... ) {
	if (!require(rgdal)) { stop() }

	rownr <- rowFromCell(raster, dataIndices(raster)[1])
	if ( rownr == 1) {
		transient <- .getGDALtransient(raster, filename=filename, mvFlag=mvFlag, options=options, ...)
		attr(raster@file, "transient") <- transient
		raster@file@driver <- 'gdal'
		raster@data@source <- 'disk'		
		.setFilename(raster) <- filename
	}	
	
#	raster@data@values[is.nan(raster@data@values)] <- NA
#	raster@data@values[is.infinite(raster@data@values)] <- NA
#	if (raster@file@dtype == "INT" || raster@file@dtype =='LOG' ) { 
#		values <- as.integer(round(raster@data@values))  
#		values[is.na(values)] <- as.integer(raster@file@nodatavalue)		
#	} else { 
#		values  <- as.numeric( raster@data@values ) 
#	}
	
	if (!raster@data@haveminmax) {
		rsd <- na.omit(raster@data@values) # min and max values
		if (length(rsd) > 0) {
			raster@data@min <- min(raster@data@min, min(rsd))
			raster@data@max <- max(raster@data@max, max(rsd))
		}	
	}
	
    for (band in 1:nlayers(raster)) {
		x <- putRasterData(raster@file@transient, values(raster, rownr), band, c((rownr-1), 0)) 
	}
	if ( rownr == nrow(raster)) {
		saveDataset(raster@file@transient, filename )
		GDAL.close(raster@file@transient) 
		
		# establish the handle:
		rasterout <- raster(filename)
		
		if (!raster@data@haveminmax) {
			rasterout@data@min <- raster@data@min
			rasterout@data@max <- raster@data@max
		}
		rasterout@data@haveminmax <- TRUE
		raster@file@driver <- 'gdal'
		
		.writeStx(rasterout) 
		return(rasterout)
	}
	return(raster)
}
