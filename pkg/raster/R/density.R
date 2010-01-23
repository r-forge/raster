# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: December 2009
# Version 0.1
# Licence GPL v3


if (!isGeneric("density")) {
	setGeneric("density", function(x, ...)
		standardGeneric("density"))
}	

setMethod('density', signature(x='RasterLayer'), 
	function(x, maxpixels=100000, main='', ...) {
		d = sampleRegular(x, maxpixels)
		plot(density(na.omit(d)), main=main, ...)
	}
)
 
