# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  October 2008
# Version 0.8
# Licence GPL v3


xmin <- function(object) {
	object <- extent(object)
	return(object@xmin)
}

xmax <- function(object) {
	object <- extent(object)
	return(object@xmax)
}

ymin <- function(object) {
	object <- extent(object)
	return( object@ymin)
}

ymax <- function(object) {
	object <- extent(object)
	return (object@ymax)
}

xres <- function(object) {
	return ( (xmax(object) - xmin(object)) / ncol(object))  
}

yres <- function(object) {
	return ( (ymax(object) - ymin(object)) / nrow(object))  
}

res <- function(object) {
	return(c(xres(object), yres(object)))
}


origin <- function(object) {
	x <- xmin(object) - xres(object)*(round(xmin(object) / xres(object)))
	y <- ymax(object) - yres(object)*(round(ymax(object) / yres(object)))
	return(c(x, y))
}

