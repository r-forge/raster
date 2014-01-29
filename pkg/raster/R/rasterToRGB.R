# Author: Robert J. Hijmans
# Date :  April 2010
# Version 0.9
# Licence GPL v3

# partly based on functions in the pixmap package by Friedrich Leisch

if (!isGeneric("RGB")) {
	setGeneric("RGB", function(x, ...)
		standardGeneric("RGB"))
}	


setMethod("RGB", signature(x='RasterLayer'), 
function(x, filename='', col=rainbow(10), ext=NULL, colNA='white', breaks=NULL, zlim=NULL, zlimcol=NULL, ...) { 

	if (!is.null(ext)) {
		x <- crop(x, ext)
	}
	
#	if (!missing(alpha)) {
#		out <- brick(x, nl=4, values=FALSE)
#		names(out) <- c('red', 'green', 'blue', 'alpha')
#	} else {
	out <- brick(x, nl=3, values=FALSE)
	names(out) <- c('red', 'green', 'blue')
#	}

	e <- as.vector(t(bbox(extent(x))))
	x <- as.matrix(x)
	x[is.infinite(x)] <- NA
	if (!is.null(zlim)) {
		if (!is.null(zlimcol)) {
			x[x < zlim[1]] <- zlim[1]
			x[x > zlim[2]] <- zlim[2]
		} else { #if (is.na(zlimcol)) {
			x[x < zlim[1] | x > zlim[2]] <- NA
		} 
	}
	
	w <- getOption('warn')
	options('warn'=-1) 
	if (is.null(breaks)) {
		zrange <- range(x, zlim, na.rm=TRUE)
	} else {
		zrange <- range(x, zlim, breaks, na.rm=TRUE)
	}
	options('warn'=w) 
	
	
	if (! is.finite(zrange[1])) {
		legend <- FALSE 
	} else {
		x <- .asRaster(x, col, breaks, fun, zrange, colNA)
	}
	
	x <- as.vector(x)
	
#	if (!missing(alpha)) {
#		x <- t(col2rgb(x, alpha=TRUE))
#	} else {
	x <- t(col2rgb(x, alpha=FALSE))	
#	}
	out <- setValues(out, x)
	
	if (filename != '') {
		out <- writeRaster(out, filename, datatype='INT2U', ...)
	} 
	
	return(out)

}
)

#x = raster(nr=10, nc=10)
#x[] = 1:100
#y = RGB(x)
#plotRGB(y)



