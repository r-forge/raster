# R function for the raster package
# Author: Robert J. Hijmans
# International Rice Research Institute. Philippines
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.8
# Licence GPL v3



click <- function(object, n=1, xy=FALSE, type = "n", ...) {
	loc <- locator(n)
	x <- loc$x
	y <- loc$y
	xy <- cbind(x, y)
	if (dataContent(object) != 'all') {
		if (dataSource(object) != 'disk') {
			stop('no data associated with this RasterLayer object')
		} else {
			return(xyValues(object, xy))
		}	
	} else {
		cell <- cellFromXY(object, xy)
		value <- values(object)[cell]
		return(cbind(xy, value)) 
	}	
}

