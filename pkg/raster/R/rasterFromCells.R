# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  April 2009
# Version 0.8
# Licence GPL v3


rasterFromCells <- function(raster, cells) {
	x <- unique(cells)
	cols <- colFromCell(raster, x)
	rows <- rowFromCell(raster, x)
	res <- resolution(raster)
	x1 <- xFromCol(raster, min(cols)) - 0.5 * res[1]
	x2 <- xFromCol(raster, max(cols)) + 0.5 * res[1]
	y1 <- yFromRow(raster, max(rows)) - 0.5 * res[2]
	y2 <- yFromRow(raster, min(rows)) + 0.5 * res[2]
	bb <- newBbox(x1, x2, y1, y2)
	raster <- clearValues(raster)
	cells2 <- cellsFromBbox(raster, bb)
	r <- crop(raster, bb)
	r[] <- cells2
	return(r)
}

