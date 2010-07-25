# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  July 2010
# Version 0.9
# Licence GPL v3


if (!isGeneric("plotCT")) {
	setGeneric("plotCT", function(x, ...)
		standardGeneric("plotCT"))
}	

setMethod("plotCT", signature(x='RasterLayer'), 
function(x, coltab=NULL, maxpixels=100000, extent=NULL, axes=TRUE, xlab='', ylab='', asp, interpolate=FALSE, ...) { 
	
 	if (missing(asp)) {
		if (.couldBeLonLat(x)) {
			ym <- mean(x@extent@ymax + x@extent@ymin)
			asp <- min(5, 1/cos((ym * pi)/180))
			asp = NA
		} else {
			asp = 1
		}		
	}


	if (is.null(coltab) &  fromDisk(x) ) {
		if (.requireRgdal()) {
			coltab <- getColorTable( GDAL.open(filename(x)) )
			if (length(coltab) == 0) coltab <- NULL
		}
	}

	r <- sampleRegular(x, maxpixels, extent=extent, asRaster=TRUE, corners=TRUE)
	z <- getValues(r) + 1

	if (! is.null(coltab) ) {
		z <- matrix(coltab[z], nrow=nrow(r), ncol=ncol(r), byrow=T)
		z <- as.raster(z)
	} else {
		z <- matrix(z, nrow=nrow(r), ncol=ncol(r), byrow=T)
		z <- as.raster(z, max=max(z, na.rm=TRUE))
	}

	require(grDevices)
	plot(c(xmin(r), xmax(r)), c(ymin(r), ymax(r)), type = "n", xlab=xlab, ylab=ylab, asp=asp, axes=axes, ...)
	rasterImage(z, xmin(r), ymin(r), xmax(r),  ymax(r), interpolate=interpolate)
}
)

