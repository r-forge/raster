# R function for the raster package
# Author: Robert J. Hijmans
# International Rice Research Institute. Philippines
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.8
# Licence GPL v3



cellsFromBbox <- function(object, bndbox, expand=FALSE) {
	bndbox <- getBbox(bndbox)
#	bndbox@xmax - 0.01 * xres(object)
#	bndbox@ymin - 0.01 * yres(object)
	
	innerBox <- intersectBbox(getBbox(object), bndbox)

	srow <- rowFromY(object, innerBox@ymax)
	
	if (trunc((ymin(object) - bndbox@ymin)/yres(object)) == (ymin(object) - bndbox@ymin)/yres(object)) { 
		bndbox@ymin <- bndbox@ymin + 0.5 * yres(object) 
	}
	erow <- rowFromY(object, innerBox@ymin)
	
	scol <- colFromX(object, innerBox@xmin)
	if (trunc((xmax(object) - bndbox@xmax)/xres(object)) == (xmax(object) - bndbox@xmax)/xres(object)) { 
		bndbox@xmax <- bndbox@xmax - 0.5 * xres(object) 
	}
	ecol <- colFromX(object, innerBox@xmax)

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

