# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  April 2009
# Version 0.8
# Licence GPL v3


rasterFromCells <- function(object, cells) {
	x <- unique(cells)
	cols <- colFromCell(object, x)
	rows <- rowFromCell(object, x)
	res <- res(object)
	x1 <- xFromCol(object, min(cols)) - 0.5 * res[1]
	x2 <- xFromCol(object, max(cols)) + 0.5 * res[1]
	y1 <- yFromRow(object, max(rows)) - 0.5 * res[2]
	y2 <- yFromRow(object, min(rows)) + 0.5 * res[2]
	bb <- newExtent(x1, x2, y1, y2)
	object <- clearValues(object)
	cells2 <- cellsFromBbox(object, bb)
	r <- crop(object, bb)
	r[] <- cells2
	return(r)
}

