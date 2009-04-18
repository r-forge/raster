
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,1
# Licence GPL v3



.writeAscii <- function(raster, overwrite=FALSE) {
	filename <- trim(raster@file@name)
	if (filename == "") {
		stop('first provide a filename. E.g.: filename(raster) <- "c:/myfile"')
	}
	
	if (dataIndices(raster)[1] == 1) {
		resdif <- abs((yres(raster) - xres(raster)) / yres(raster) )
		if (resdif > 0.01) {
			stop(paste("raster has unequal horizontal and vertical resolutions","\n", "these data cannot be stored in arc-ascii format"))
		}
		if (!overwrite & file.exists(filename)) {
				stop(paste(filename, "exists. Use 'overwrite=TRUE'")) 
		}

		thefile <- file(filename, "w")  # open an txt file connection
		cat("NCOLS", ncol(raster), "\n", file = thefile)
		cat("NROWS", nrow(raster), "\n", file = thefile)
		cat("XLLCORNER", xmin(raster), "\n", file = thefile)
		cat("YLLCORNER", ymin(raster), "\n", file = thefile)
		cat("CELLSIZE",  xres(raster), "\n", file = thefile)
		cat("NODATA_value", .nodatavalue(raster), "\n", file = thefile)
		close(thefile) #close connection
		
    } else if ( dataIndices(raster)[2] > ncell(raster)) {
		stop(paste('writing beyond end of file. last cell:', dataIndices(raster)[2], '>', ncell(raster)))
	}

	
	raster@data@values[is.na(values(raster))] <- .nodatavalue(raster)
	if (dataContent(raster) == 'all') {
		for (r in 1:nrow(raster)) {
			write.table(t(valuesRow(raster, r)), filename, append = TRUE, quote = FALSE, 
								sep = " ", eol = "\n", dec = ".", row.names = FALSE, col.names = FALSE)
		}					
	} else {
		write.table(t(values(raster)), filename, append = TRUE, quote = FALSE, 
							sep = " ", eol = "\n", dec = ".", row.names = FALSE, col.names = FALSE)
    }
	
	if ( dataIndices(raster)[2] == ncell(raster)) {
		return(raster(filename))
	} else {
		return(raster)
	}	
}
 
 