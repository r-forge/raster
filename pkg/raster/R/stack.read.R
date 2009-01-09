# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : June 2008
# Version 0,1
# Licence GPL v3


.stackRead <- function(rstack, rownumber, startcol=1, ncolumns=(ncol(rstack)-startcol+1)) {
	for (i in 1:length(rstack@rasters)) {
		raster <- readPartOfRow(rstack@rasters[[i]], rownumber, startcol, ncolumns)
		if ( i == 1 )  {
			rstack@data@values <- as.matrix(values(raster))
		}
		else {
			rstack@data@values <- cbind(rstack@data@values, values(raster)) 
		}	   
	}
	rstack@data@content <- dataContent(raster)
	rstack@data@indices <- dataIndices(raster)
	return(rstack)
}


.stackReadXY <- function(rasterstack, xy) {
	cells <- cellFromXY(rasterstack, xy)
	return(.stackReadCells(rasterstack, cells))
}


.stackReadCells <- function(rasterstack, cells) {
	for (i in 1:nlayers(rasterstack)) {
		v <- .rasterReadCells (rasterstack@rasters[[i]], cells)
		if (i == 1) {
			result <- v
		} else {
			result <- cbind(result, v)
#			colnames(result)[length(result[1,])] <- rstack@rasters[[i]]@file@shortname
		}
	}
	if (!(is.null(dim(result)))) {
		for (i in 1:nlayers(rasterstack)) {
			label <- rasterstack@rasters[[i]]@file@shortname
			if (nchar(label) == "") { 
				label <- paste("raster_", i, sep="") 
			}
			colnames(result)[i] <- label
		}
	}	
	return(result)
}


