# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  October 2008
# Version 0.8
# Licence GPL v3


yFromRow <- function(object, rownr) {
	if (.isSPgrid(object)) { object <- asRasterLayer(object, FALSE) }
	rownr <- round(rownr)
	rownr[rownr < 1 | rownr > nrow(object)] <- NA
	y <- ymax(object) - ((rownr-0.5) * yres(object))
	#hello
	return(y) }	
	
	
xFromCol <- function(object, colnr) {
	if (.isSPgrid(object)) { object <- asRasterLayer(object, FALSE) }
	colnr <- round(colnr)
	colnr[colnr < 1 | colnr > ncol(object)] <- NA
	x <- xmin(object) + (colnr - 0.5) * xres(object) 
	return(x) }  


cellFromXY <- function(object, xy) {
	if (.isSPgrid(object)) { object <- asRasterLayer(object, FALSE) }
	if (class(xy) == 'SpatialPoints' | class(xy) == 'SpatialPointsDataFrame') {
		x <- coordinates(xy)[,1]
		y <- coordinates(xy)[,2]
	} else if (is.null(dim(xy))) { 
		x <- xy[1]
		y <- xy[2] 
	} else { 
		x <- xy[,1]
		y <- xy[,2] 
	}
	cell <- vector(mode = "integer", length = length(x))
	cell[] <- NA
	for (i in seq(length(x))) {
		colnr <- colFromX(object, x[i]) - 1
		rownr <- rowFromY(object, y[i]) - 1
		if ((!is.na(colnr)) & (!is.na(rownr))) {
			cell[i] <- rownr * ncol(object) + colnr + 1
		}
	}
	return(cell)
}

colFromX <- function ( object, x )	{
	if (.isSPgrid(object)) { object <- asRasterLayer(object, FALSE) }
	if (class(x) == 'SpatialPoints' | class(x) == 'SpatialPointsDataFrame') {	x <- x@points[,1] }
	colnr <- (trunc((x - xmin(object)) / xres(object))) + 1 
	colnr[x == xmax(object)] <- ncol(object)
	colnr[x < xmin(object) | x > xmax(object) ] <- NA
	return(as.vector(colnr))
}
	
	
rowFromY <- function ( object, y )	{
	if (.isSPgrid(object)) { object <- asRasterLayer(object, FALSE) }
	if (class(y) == 'SpatialPoints' | class(y) == 'SpatialPointsDataFrame') {	y <- y@points[,2] }
	rownr <- 1 + (trunc((ymax(object) - y) / yres(object)))
	rownr[y == ymin(object) ] <- nrow(object) 
	rownr[y > ymax(object) | y < ymin(object)] <- NA
	return(rownr)
}	
	

xyFromCell <- function(object, cell, asSpatialPoints=FALSE) {
	if (.isSPgrid(object)) { object <- asRasterLayer(object, FALSE) }
	cell <- round(cell)
	xy <- matrix(data = NA, ncol=2, nrow=length(cell))
	colnr <- colFromCell(object, cell)
	rownr <- rowFromCell(object, cell)
	xy[,1] <- xFromCol(object, colnr)
	xy[,2] <- yFromRow(object, rownr) 		
	colnames(xy) <- c("x", "y")
	if (asSpatialPoints) {
		xy <- SpatialPoints(xy, projection(object, asText=FALSE))
	}
	return(xy)
}  
	

	
cxyFromBbox <- function(object, bbox) {
	if (.isSPgrid(object)) { object <- asRasterLayer(object, FALSE) }
	bbox <- getBbox(bbox)
	cells <- cellsFromBbox(object, bbox)
	cxy <- cbind(cells, xyFromCell(object, cells))
	colnames(cxy) <- c("cell", "x", "y")
	return(cxy)
}

