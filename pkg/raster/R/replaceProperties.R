# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3



'ncol<-' <- function(x, value) {
	return( setRowCol(x, ncols=value) )
}	

'nrow<-' <- function(x, value) {
	return( setRowCol(x, nrows=value) )
}	

'resolution<-' <- function(x, value) {
	if (length(value) == 1) {
		return( setRes(x, xres=value, yres=value) )
	} else {
		return( setRes(x, xres=value[1], yres=value[2]) )
	}
}

'xmin<-' <- function(x, value) {
	x@bbox@xmin <- value
	return(x)
}

'xmax<-' <- function(x, value) {
	x@bbox@xmax <- value
	return(x)
}

'ymin<-' <- function(x, value) {
	x@bbox@ymin <- value
	return(x)
}

'ymax<-' <- function(x, value) {
	x@bbox@ymax <- value
	return(x)
}

