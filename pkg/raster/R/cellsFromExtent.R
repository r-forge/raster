# R function for the raster package
# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.9
# Licence GPL v3



cellsFromExtent <- function(object, extent, expand=FALSE) {
	
	bndbox <- extent(extent)
	bndbox <- alignExtent(bndbox, object)
	innerBox <- intersectExtent(extent(object), bndbox)
	
	srow <- rowFromY(object, innerBox@ymax)
	erow <- rowFromY(object, innerBox@ymin + 0.5 * yres(object))
	scol <- colFromX(object, innerBox@xmin)
	ecol <- colFromX(object, innerBox@xmax - 0.5 * xres(object))
	
	if (expand) {
		addrowstop <- as.integer(round((bndbox@ymax - innerBox@ymax) / yres(object)))
		addrowsbot <- as.integer(round((innerBox@ymin - bndbox@ymin) / yres(object)))
		addcolsleft <- as.integer(round((innerBox@xmin - bndbox@xmin) / xres(object)))
		addcolsright <- as.integer(round((bndbox@xmax - innerBox@xmax) / xres(object)))
		nc <- ecol-scol+1+addcolsleft+addcolsright
	}

	cell <- vector()
	if (expand && addrowstop > 0) {
		cell <- rep(NA, nc * addrowstop)
	}

	if (erow > srow) {
		if (expand && max( addcolsleft, addcolsright) > 0) {
			for (r in (srow):erow) {
				cell2 <- cellFromRowCol(object, r, scol):cellFromRowCol(object, r, ecol)
				if (addcolsleft > 0) {
					cell2 <- c(rep(NA, addcolsleft), cell2)
				} 
				if (addcolsright > 0) {
					cell2 <- c(cell2, rep(NA, addcolsright))
				} 
				cell <- c(cell, cell2)
			}
		} else {
	# ouch, vectorize, please
			for (r in (srow):erow) {
				cell2 <- cellFromRowCol(object, r, scol):cellFromRowCol(object, r, ecol)
				cell <- c(cell, cell2)
			}
		}
	}

	if (expand && addrowsbot > 0) {
		cell <- c(cell, rep(NA, nc * addrowsbot))
	}

	return(cell)
}

