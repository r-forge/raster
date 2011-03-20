

.img <- function(x, maxpixels=500000, col=rainbow(25), interpolate=FALSE, axes=TRUE, xlab='', ylab='', extent=NULL, alpha=1, classes=FALSE, add=FALSE, ...) {

	if (!add) {
		if (!axes) par(plt=c(0,1,0,1))
	}
	
	#	if (missing(asp)) {
	#		if (raster:::.couldBeLonLat(x)) {
	#			ym <- mean(x@extent@ymax + x@extent@ymin)
	#			asp <- min(5, 1/cos((ym * pi)/180))
	#			asp = NA
	#		} else {
	#			asp = 1
	#		}
	#	}
	

	if (alpha < 1) {
		alpha <- max(alpha, 0) * 255 + 1
		a    <- c(0:9, LETTERS[1:6])
		alpha <- paste(rep(a, each=16), rep(a, times=16), sep='')[alpha]
		col <- paste(substr(col, 1, 7), alpha, sep="")
	}

	x <- sampleRegular(x, maxpixels, extent=extent, asRaster=TRUE, corners=TRUE)
	bb <- as.vector(t(bbox(x)))
	dm <- dim(x)[1:2]
	x <- as.matrix(x)
	if (classes) {
		if (length(unique(x)) <= length(col)) {
			x <- col[x]
		}
	} else {
		x <- as.numeric(cut(x, length(col)))
		x <- col[x]
	}
	dim(x) <- dm
	if (!add) {
		plot(c(bb[1], bb[2]), c(bb[3], bb[4]), type = "n", xlab=xlab, ylab=ylab, ...)
	}
	rasterImage(x, bb[1], bb[3], bb[2], bb[4], interpolate=interpolate, ...)
	
}


#.plotr(r)

