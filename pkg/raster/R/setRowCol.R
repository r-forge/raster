# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3


'rowcol<-' <- function(x, value) {
	if (length(value) == 1) {
		value <- c(value, ncol(x))
	}
	if (value[1] != nrow(x) | value[2] != ncol(x)) {
		if (extends(class(x), "Raster")) {
			x <- clearValues(x)
		}
	}
	x@nrows <- as.integer(value[1])
	x@ncols <- as.integer(value[2])
	return(x)
}


