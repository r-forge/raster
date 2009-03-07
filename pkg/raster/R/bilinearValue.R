
.fourCellsFromXY <- function(raster, xy) {
	cells <- cellFromXY(raster, xy)
	row <- rowFromCell(raster, cells)
	col <- colFromCell(raster, cells)
	cellsXY <- xyFromCell(raster, cells)

	pos <- matrix(-1, ncol=ncol(xy), nrow=nrow(xy))
	pos[xy[,1] > cellsXY[,1]] <- 1
	pos[xy[,2] < cellsXY[,2]] <- 1
	
	four <- matrix(ncol=4, nrow=nrow(xy))
	four[,1] <- cells
	four[,2] <- cellFromRowCol(raster, row + pos[,2], col)
	four[,3] <- cellFromRowCol(raster, row + pos[,2], col + pos[,1])
	four[,4] <- cellFromRowCol(raster, row, col + pos[,1])
		
	four[is.na(four)] <- rep(four[,1], 4)[is.na(four)]
	return(four)
}


.bilinear <- function(x,y, x1,x2,y1,y2, q11,q12,q21,q22) {
	div <- (x2-x1)*(y2-y1)
	if (all(div>0)) {
		return( (q11/div)*(x2-x)*(y2-y) + (q21/div)*(x-x1)*(y2-y) + (q12/div)*(x2-x)*(y-y1) + (q22/div)*(x-x1)*(y-y1) )
	} else {
		bil <- vector(length=length(div))
		bil[div>0] <- (q11/div)*(x2-x)*(y2-y) + (q21/div)*(x-x1)*(y2-y) + (q12/div)*(x2-x)*(y-y1) + (q22/div)*(x-x1)*(y-y1) 
		bil[(x1==x2 && y1==y2)] <- q11
		div <- y2-y1
		bil[(x1==x2 && y1!=y2)] <- (q11/div)*(y2-y) + (q12/div)*(y-y1)
		div <- x2-x1
		bil[(x1!=x2 && y1==y2)] <- (q11/div)*(x2-x) + (q21/div)*(x-x1) 
	}
	return(bil)
}


if (!isGeneric("bilinearValue")) {
	setGeneric("bilinearValue", function(raster, xyCoords)
		standardGeneric("bilinearValue"))
}	

setMethod("bilinearValue", signature(raster='RasterLayer', xyCoords='matrix'), 
function(raster, xyCoords) {
	four <- .fourCellsFromXY(raster, xyCoords)
	xy4 <- matrix(xyFromCell(raster, as.vector(four)), ncol=8)
	x1 <- apply(xy4[,1:4], 1, min)
	x2 <- apply(xy4[,1:4], 1, max)
	y1 <- apply(xy4[,5:8], 1, min)
	y2 <- apply(xy4[,5:8], 1, max)
	xy4 <- cbind(c(x1, x1, x2, x2), c(y1, y2, y1, y2))
	cells <- cellFromXY(raster, xy4)
	v <- matrix(cellValues(raster, cells), ncol=4)
	return( .bilinear(xyCoords[,1], xyCoords[,2], x1, x2, y1, y2, v[,1], v[,2], v[,3], v[,4]) )
}
)
