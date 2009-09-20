# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  April 2009
# Version 0.8
# Licence GPL v3


if (!isGeneric("ncell")) {
	setGeneric("ncell", function(x)
		standardGeneric("ncell"))
}	

setMethod('ncell', signature(x='Raster'), 
	function(x) {
		d <- dim(x)
		# return numeric to avoid integer overflow
		t <- as.numeric(d[1]) * d[2]
		return(t)
	}
)



setMethod('ncell', signature(x='ANY'), 
	function(x) {
		d <- dim(x)
		if (is.null(d)) {
			return(length(x))
		}
# return numeric to avoid integer overflow
		t <- as.numeric(d[1]) * d[2]
		if (length(d) == 2) {
			return(t)
		} else {
			for (i in 3:length(d)) {
				t <- t * d[i]
			}
			return(t)
		}
	}
)

