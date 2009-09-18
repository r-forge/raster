# Author: Robert J. Hijmans, r.hijmans@gmail.com 
# Date :  January 2009
# Version 0.9
# Licence GPL v3


# helper functions for group generic functions

.getRasterValues <- function(x) {
# need to take care of 'spase'
	if (dataContent(x) != 'all') {
		if (class(x) == "RasterLayer") {
			if (dataSource(x) == 'ram') {
				stop('no data on disk or in memory')
			} else x <- readAll(x)	
		} else {
			x <- readAll(x)
		}
	}
	return(values(x))
}	

.getRowValues <- function(x, r) {
# need to take care of 'spase'
	if (dataContent(x) == 'all') {
		return(valuesRow(x, r))
	} else {	
		if (dataSource(x) == 'disk') {
			return(values(readRow(x, r)))
		} else { 
			stop('data not on disk or in memory')
		} 
	}	
}	


.getAllTypeOfValues <- function(x, y, i) {
	if ( extends(class(y), "Raster") & compare(c(x, y)) ) {			
		return(.getRasterValues(y))
	} else if (is.atomic(y)) {
		return(rep(y, ncell(x)))
	} else if (length(y)==ncell(x)) {
		return(y)
	} else {
		stop(paste("I do not understand argument",i + 1)) 
	}	
}



