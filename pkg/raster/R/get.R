# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  October 2008
# Version 0,2
# Licence GPL v3


yFromRow <- function(raster, rownr) {
	rownr <- round(rownr)
	rownr[rownr < 1 | rownr > raster@nrows] <- NA
	y <- ymax(raster) - ((rownr-0.5) * yres(raster))
	#hello
	return(y) }	
	
	
xFromCol <- function(raster, colnr) {
	colnr <- round(colnr)
	colnr[colnr < 1 | colnr > raster@ncols] <- NA
	x <- xmin(raster) + (colnr - 0.5) * xres(raster) 
	return(x) }  

	
rowFromCell <- function(raster, cell) {
	cell <- as.integer(round(cell))
	cell[cell < 1 | cell > ncells(raster)] <- NA
	rownr <- as.integer(trunc((cell-1)/raster@ncols) + 1)
#	rownr <- as.integer(trunc(cell / (raster@ncols+1)) + 1)
    return(rownr)
}


colFromCell <- function(raster, cell) {
	cell <- as.integer(round(cell))
	cell[cell < 1 | cell > ncells(raster)] <- NA	
	rownr <- as.integer(trunc((cell-1)/raster@ncols) + 1)
	colnr <- as.integer(cell - ((rownr-1) * raster@ncols))
#	colnr <- as.integer(trunc(cell - (trunc(cell / (raster@ncols+1) )) * raster@ncols))
    return(colnr)
}


	
cellFromXY <- function(raster, xy) {
	if (is.null(dim(xy))) { 
		x <- xy[1]
		y <- xy[2] 
	} else { 
		x <- xy[,1]
		y <- xy[,2] 
	}
	cell <- vector(mode = "integer", length = length(x))
	cell[cell == 0] <- NA
	for (i in 1:length(x)) {
		colnr <- colFromX(raster, x[i]) - 1
		rownr <- rowFromY(raster, y[i]) - 1
		if ((!is.na(colnr)) & (!is.na(rownr))) {
			cell[i] <- as.integer((rownr * raster@ncols + colnr) + 1)
		}
	}
	return(cell)
}


cellFromRowcol <- function(raster, rownr, colnr) {
	rownr <- round(rownr)
	colnr <- round(colnr)
	rownr[rownr < 1 | rownr > raster@nrows] <- NA
	colnr[colnr < 1 | colnr > raster@ncols] <- NA	
	return((rownr-1) * raster@ncols + colnr)
}

colFromX <- function ( raster, x )	{
	colnr <- (trunc((x - xmin(raster)) / xres(raster))) + 1 
	colnr[x == xmax(raster)] <- ncol(raster)
	colnr[x < xmin(raster) | x > xmax(raster) ] <- NA
	return(colnr) }
	
	
rowFromY <- function ( raster, y )	{
	rownr <- 1 + (trunc((ymax(raster) - y) / yres(raster)))
	rownr[y == ymin(raster) ] <- raster@nrows 
	rownr[y > ymax(raster) | y < ymin(raster)] <- NA
	return(rownr)
}	
	

xyFromCell <- function(raster, cell) {
	cell <- round(cell)
	xy <- matrix(data = NA, ncol=2, nrow=length(cell))
	colnr <- colFromCell(raster, cell)
	rownr <- rowFromCell(raster, cell)
	xy[,1] <- xFromCol(raster, colnr)
	xy[,2] <- yFromRow(raster, rownr) 		

	colnames(xy) <- c("x", "y")
	return(xy)
}  
	
	
cxyFromBox <- function(raster, xmn=xmin(raster), xmx=xmax(raster), ymn=ymin(raster), ymx=ymax(raster)) {
	firstrow <- rowFromY(raster, ymx)
	lastrow <- rowFromY(raster, ymn)
	firstcol <- colFromX(raster, xmn)
	lastcol <- colFromX(raster, xmx)
	cells <- vector("integer", length=0)
# RH: ouch, this should be done with apply 	
	for (i in firstrow:lastrow) {
		firstcell <- (i-1) * raster@ncols + firstcol
		lastcell <- (i-1) * raster@ncols + lastcol
		cells <- append(cells, c(firstcell:lastcell))
	}
	cxy <- cbind(cells, xyFromCell(raster, cells))
	colnames(cxy) <- c("cell", "x", "y")
	return(cxy)
}


validCells <- function(raster, cell) {
	cell <- round(cell)
	validcell <- vector(length=length(cell))
	validcell[cell > 0 & cell <= ncells(raster)] <- TRUE
	return(validcell)
}

validRows <- function(raster, rownr) {
	rownr <- round(rownr)
	validrows <- vector(length=length(rownr))
	validrows[rownr > 0 & rownr <= nrow(raster)] <- TRUE
	return(validrows)
}


validCols <- function(raster, colnr) {
	colnr <- round(colnr)
	validcols <- vector(length=length(colnr))
	validcols[colnr > 0 & colnr <= nrow(raster)] <- TRUE
	return(validcols)
}
