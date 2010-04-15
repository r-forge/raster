# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  April 2010
# Version 0.9
# Licence GPL v3

# based on functions in the pixmap package by Friedrich Leisch

if (!isGeneric("plotRGB")) {
	setGeneric("plotRGB", function(x, ...)
		standardGeneric("plotRGB"))
}	

setMethod("plotRGB", signature(x='Raster'), 
function(x, r=1, g=2, b=3, maxpixels=100000, ...) { 
	r <- sampleRegular(raster(x,r), maxpixels, asRaster=TRUE)
	g <- sampleRegular(raster(x,g), maxpixels, asRaster=TRUE)
	b <- sampleRegular(raster(x,b), maxpixels, asRaster=TRUE)
	rm(x)
	x <- xFromCol(r, 1:ncol(r))
	y <- yFromRow(r, nrow(r):1)
	z <- rgb(getValues(r), getValues(g), getValues(b), max=255)
	col <- unique(z)
	z <- match(z, col)
	z <- matrix(z, nrow=nrow(r), ncol=ncol(r), byrow=T)
	z <- t(z[nrow(z):1,])
	image(x=x, y=y, z=z, col=col, ...)
}
)


