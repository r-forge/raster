# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: December 2009
# Version 0.1
# Licence GPL v3


if (!isGeneric("density")) {
	setGeneric("density", function(x, ...)
		standardGeneric("density"))
}	

setMethod('density', signature(x='RasterLayer'), 
	function(x, maxpixels=100000, plot=TRUE, main='', ...) {
		d = sampleRegular(x, maxpixels)
		x = density(na.omit(d))
		if (plot) {
			plot(x, main=main, ...)
		} else {
			return(x)
		}
	}
)
 
