# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  October 2008
# Version 0,2
# Licence GPL v3


.isSPgrid <- function(object){
	if (class(object) == "SpatialGrid" | class(object) == "SpatialGridDataFrame" | class(object) == "SpatialPixels" | class(object) == "SpatialPixelsDataFrame") {
		return(TRUE)
	} else {
		return(FALSE)
	}	
}

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

	
rowFromCell <- function(object, cell) {
	if (.isSPgrid(object)) { object <- asRasterLayer(object, FALSE) }
	cell <- as.integer(round(cell))
	cell[cell < 1 | cell > ncells(object)] <- NA
	rownr <- as.integer(trunc((cell-1)/ncol(object)) + 1)
    return(rownr)
}


colFromCell <- function(object, cell) {
	if (.isSPgrid(object)) { object <- asRasterLayer(object, FALSE) }
	cell <- as.integer(round(cell))
	cell[cell < 1 | cell > ncells(object)] <- NA	
	rownr <- as.integer(trunc((cell-1)/ncol(object)) + 1)
	colnr <- as.integer(cell - ((rownr-1) * ncol(object)))
    return(colnr)
}


	
cellFromXY <- function(object, xy) {
	if (.isSPgrid(object)) { object <- asRasterLayer(object, FALSE) }
	if (class(xy) == 'SpatialPoints' | class(xy) == 'SpatialPointsDataFrame') {
		x <- xy@points[,1]
		y <- xy@points[,2]
	} else if (is.null(dim(xy))) { 
		x <- xy[1]
		y <- xy[2] 
	} else { 
		x <- xy[,1]
		y <- xy[,2] 
	}
	cell <- vector(mode = "integer", length = length(x))
	cell[] <- NA
	for (i in 1:length(x)) {
		colnr <- colFromX(object, x[i]) - 1
		rownr <- rowFromY(object, y[i]) - 1
		if ((!is.na(colnr)) & (!is.na(rownr))) {
			cell[i] <- as.integer((rownr * ncol(object) + colnr) + 1)
		}
	}
	return(cell)
}


cellFromRowcol <- function(object, rownr, colnr) {
	if (.isSPgrid(object)) { object <- asRasterLayer(object, FALSE) }
	rownr <- round(rownr)
	colnr <- round(colnr)
	rownr[rownr < 1 | rownr > nrow(object)] <- NA
	colnr[colnr < 1 | colnr > ncol(object)] <- NA	
	return((rownr-1) * ncol(object) + colnr)
}

colFromX <- function ( object, x )	{
	if (.isSPgrid(object)) { object <- asRasterLayer(object, FALSE) }
	if (class(x) == 'SpatialPoints' | class(x) == 'SpatialPointsDataFrame') {	x <- x@points[,1] }
	colnr <- (trunc((x - xmin(object)) / xres(object))) + 1 
	colnr[x == xmax(object)] <- ncol(object)
	colnr[x < xmin(object) | x > xmax(object) ] <- NA
	return(colnr) 
}
	
	
rowFromY <- function ( object, y )	{
	if (.isSPgrid(object)) { object <- asRasterLayer(object, FALSE) }
	if (class(y) == 'SpatialPoints' | class(y) == 'SpatialPointsDataFrame') {	y <- y@points[,2] }
	rownr <- 1 + (trunc((ymax(object) - y) / yres(object)))
	rownr[y == ymin(object) ] <- nrow(object) 
	rownr[y > ymax(object) | y < ymin(object)] <- NA
	return(rownr)
}	
	

xyFromCell <- function(object, cell, asSpatialPoints=FALSE, projstring="") {
	if (.isSPgrid(object)) { object <- asRasterLayer(object, FALSE) }
	cell <- round(cell)
	xy <- matrix(data = NA, ncol=2, nrow=length(cell))
	colnr <- colFromCell(object, cell)
	rownr <- rowFromCell(object, cell)
	xy[,1] <- xFromCol(object, colnr)
	xy[,2] <- yFromRow(object, rownr) 		
	colnames(xy) <- c("x", "y")
	if (asSpatialPoints) {
		xy <- SpatialPoints(xy, newCRS(projstring))
	}
	return(xy)
}  
	
	
cxyFromBox <- function(object, xmn=xmin(object), xmx=xmax(object), ymn=ymin(object), ymx=ymax(object)) {
	if (.isSPgrid(object)) { object <- asRasterLayer(object, FALSE) }
	firstrow <- rowFromY(object, ymx)
	lastrow <- rowFromY(object, ymn)
	firstcol <- colFromX(object, xmn)
	lastcol <- colFromX(object, xmx)
	cells <- vector("integer", length=0)
# RH: ouch, this should be done with apply 	
	for (i in firstrow:lastrow) {
		firstcell <- (i-1) * ncol(object) + firstcol
		lastcell <- (i-1) * ncol(object) + lastcol
		cells <- append(cells, c(firstcell:lastcell))
	}
	cxy <- cbind(cells, xyFromCell(object, cells))
	colnames(cxy) <- c("cell", "x", "y")
	return(cxy)
}


validCells <- function(object, cell) {
	if (.isSPgrid(object)) { object <- asRasterLayer(object, FALSE) }
	cell <- round(cell)
	validcell <- vector(length=length(cell))
	validcell[cell > 0 & cell <= ncells(object)] <- TRUE
	return(validcell)
}

validRows <- function(object, rownr) {
	if (.isSPgrid(object)) { object <- asRasterLayer(object, FALSE) }
	rownr <- round(rownr)
	validrows <- vector(length=length(rownr))
	validrows[rownr > 0 & rownr <= nrow(object)] <- TRUE
	return(validrows)
}

validCols <- function(object, colnr) {
	if (.isSPgrid(object)) { object <- asRasterLayer(object, FALSE) }
	colnr <- round(colnr)
	validcols <- vector(length=length(colnr))
	validcols[colnr > 0 & colnr <= nrow(object)] <- TRUE
	return(validcols)
}
