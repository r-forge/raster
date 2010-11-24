# Author: Robert J. Hijmans, r.hijmans@gmail.com 
# Date :  November 2010
# Version 1.0
# Licence GPL v3
 

if (!isGeneric("boxplot")) {
	setGeneric("boxplot", function(x, ...)
		standardGeneric("boxplot"))
}

setMethod('boxplot', signature(x='Raster'), 
	function(x, maxpixels=100000, ...) {
		d = sampleRegular(x, maxpixels)
		if (nlayers(x) == 1) {
			d <- matrix(d)
		}
		colnames(d) <- layerNames(x)
		boxplot(d, ...)
	}
)

