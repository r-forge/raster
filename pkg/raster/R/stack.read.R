# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : June 2008
# Version 0,1
# Licence GPL v3

.rasterstack.read.all <- function(rstack) {
	rstack <- .rasterstack.read.part.of.row(rstack, rownumber=-1)
	return(rstack)
}


.rasterstack.read.row <- function(rstack, rownumber) {
	return(.rasterstack.read.part.of.row(rstack, rownumber))
}


.rasterstack.read.part.of.row <- function(rstack, rownumber, startcol=1, ncolumns=(ncol(rstack)-startcol+1)) {
	for (i in 1:length(rstack@rasters)) {
		rs <- .raster.read(rstack@rasters[[i]], rownumber, startcol, ncolumns)
		if ( i == 1 )  {
			rstack@data@values <- as.matrix( values(rs) )
		}
		else {
			rstack@data@values <- cbind(rstack@data@values, values(rs)) 
		}	   
	}
	rstack@data@content <- dataContent(rs)
	rstack@data@indices <- dataIndices(rs)
	return(rstack)
}


.rasterstack.read.xy <- function(rasterstack, xy) {
	cells <- cellFromXY(rasterstack, xy)
	return(.rasterstack.read.cells(rasterstack, cells))
}


.rasterstack.read.cells <- function(rasterstack, cells) {
	for (i in 1:nlayers(rasterstack)) {
		v <- .read.cells.raster(rasterstack@rasters[[i]], cells)
		if (i == 1) {
			result <- v
		} else {
			result <- cbind(result, v[,2])
#			colnames(result)[length(result[1,])] <- rstack@rasters[[i]]@file@shortname
		}
	}
	return(result)
}


