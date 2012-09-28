# Author: Robert J. Hijmans
# Date :  September 2012
# Version 1.0
# Licence GPL v3

if (!isGeneric("barplot")) {
	setGeneric("barplot", function(height,...)
		standardGeneric("barplot"))
}	


setMethod('barplot', 'RasterLayer', 
	function(height, maxpixels=1000000, digits=0, cuts=NULL, col=rainbow, ...)  {
		x <- sampleRegular(height, maxpixels)
		if (!is.null(cuts)) {
			x <- cut(x, cuts)
		} else {
			x <- round(x, digits)
		}
		x <- table(x)
		if (is.function(col)) {
			col <- col(length(x))
		}
		barplot(x, col=col, ...)
	}
)


#f <- system.file("external/test.grd", package="raster")
#f
#r <- raster(f)
#barplot(r, cuts=10, col=rainbow(10))
