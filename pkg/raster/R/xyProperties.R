# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  October 2008
# Version 0.9
# Licence GPL v3

xmin <- function(object) {
	return(extent(object)@xmin)
}

xmax <- function(object) {
	return(extent(object)@xmax)
}

ymin <- function(object) {
	return( extent(object)@ymin)
}

ymax <- function(object) {
	return (extent(object)@ymax)
}

xres <- function(object) {
	if (!inherits(object, 'BasicRaster')) { object <- raster(object) }
	e <- object@extent
	return ( (e@xmax - e@xmin) / object@ncols )  
}

yres <- function(object) {
	if (!inherits(object, 'BasicRaster')) { object <- raster(object) }
	e <- object@extent
	return ( (e@ymax - e@ymin) / object@nrows )  
}

res <- function(object) {
	if (!inherits(object, 'BasicRaster')) { object <- raster(object) }
	return( c(xres(object), yres(object)))
}


origin <- function(object) {
	if (!inherits(object, 'BasicRaster')) { object <- raster(object) }
	e <- object@extent
	r <- c(xres(object), yres(object))
	x <- e@xmin - r[1]*(round(e@xmin / r[1]))
	y <- e@ymax - r[2]*(round(e@ymax / r[2]))
	return(c(x, y))
}

