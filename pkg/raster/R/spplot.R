# Author: Robert J. Hijmans
# Date :  June 2011
# Version 1.0
# Licence GPL v3


if (!isGeneric("spplot")) {
	setGeneric("spplot", function(obj, ...)
		standardGeneric("spplot"))
}	


setMethod("spplot", signature(obj='Raster'), 
	function(obj, maxpixels=50000, ...)  {
		if (missing(names.attr)) {
			names.attr <- layerNames(obj)
		}
		obj <- sampleRegular(obj, maxpixels, asRaster=T)
		obj <- as(obj, 'SpatialGridDataFrame')
		#obj@data <- obj@data[, ncol(obj@data):1]
		spplot(obj, ...)
	}
)

