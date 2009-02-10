# R function for the raster package
# Author: Robert J. Hijmans
# International Rice Research Institute. Philippines
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.8
# Licence GPL v3



cellsFromBbox <- function(object, bndbox) {
	bndbox <- getBbox(bndbox)
#	bndbox@xmax - 0.01 * xres(object)
#	bndbox@ymin - 0.01 * yres(object)
	srow <- rowFromY(object, bndbox@ymax)
	if (trunc((ymin(object) - bndbox@ymin)/yres(object)) == (ymin(object) - bndbox@ymin)/yres(object)) { 
		bndbox@ymin <- bndbox@ymin + 0.5 * yres(object) 
	}
	erow <- rowFromY(object, bndbox@ymin)
	scol <- colFromX(object, bndbox@xmin)
	if (trunc((xmax(object) - bndbox@xmax)/xres(object)) == (xmax(object) - bndbox@xmax)/xres(object)) { 
		bndbox@xmax <- bndbox@xmax - 0.5 * xres(object) 
	}
	ecol <- colFromX(object, bndbox@xmax)
	cell <- cellFromRowCol(object, srow, scol):cellFromRowCol(object, srow, ecol)
	if (erow > srow) {
	# ouch, vectorize, please
		for (r in (srow+1):erow) {
			cell2 <- cellFromRowCol(object, r, scol):cellFromRowCol(object, r, ecol)
			cell <- c(cell, cell2)
		}
	}
	return(cell)
}

