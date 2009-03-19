# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3


'dataType<-' <- function(x, value) {
	return( setDatatype(x, value) )
}


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
	return(changeExtent(x, xmn=value))
}

'xmax<-' <- function(x, value) {
	return(changeExtent(x, xmx=value))
}

'ymin<-' <- function(x, value) {
	return(changeExtent(x, ymn=value))
}

'ymax<-' <- function(x, value) {
	return(changeExtent(x, ymx=value))
}

