# Author: Robert J. Hijmans
# Date : February 2010
# Version 1.0
# Licence GPL v3


slopeAspect <- function(alt, filename='', type='', unit='', rotateAspect=FALSE, ...) {
	
	type <- trim(tolower(type))
	stopifnot(type %in% c('', 'both' , 'slope', 'aspect'))
	unit <- tolower(unit)
	stopifnot(unit %in% c('degrees', ''))
	filename <- trim(filename)
	
	res <- res(alt)
	xres <- res[1]
	yres <- res[2]
	fX <- matrix(c(-1,-2,-1,0,0,0,1,2,1), nrow=3) * -1
	fY <- matrix(c(-1,0,1,-2,0,2,-1,0,1), nrow=3) 
	
	if (raster:::.couldBeLonLat(alt)) {
		yres <- pointDistance(cbind(0,0), cbind(0,yres), longlat=TRUE)
		fY <- fY / yres
		zy <- focalFilter(alt, fY)
		zx <- focalFilter(alt, fX)
		
		y <- yFromRow(alt, 1:nrow(alt))
		dx <- raster:::.haversine(-xres, y, xres, y) / 3
		zx <- t( t(zx) / dx)
		
	} else {
	
		fX <- fX / xres
		fY <- fY / yres
		zx <- focalFilter(alt, fX)
		zy <- focalFilter(alt, fY)
	}

	if (type == 'slope') {
		
		x <- sqrt( zy^2 + zx^2 ) 
		if (unit == 'degrees') {
			x = atan(x) / (pi / 180)
		}
		layerNames(x) <- 'slope'
		
	} else if (type == 'aspect') {
		x <- atan2(zy, zx)
		if (rotateAspect) {
			x = x %%(2*pi) 
		}
		if (unit == 'degrees') {
			x <- x / (pi / 180)
		}
		layerNames(x) <- 'aspect'
		
	} else {
		slope <- sqrt( zy^2 + zx^2 ) 
		aspect <- atan2(zy, zx) 
		if (rotateAspect) {
			aspect = aspect %%(2*pi) 
		}
		
		if (unit == 'degrees') {
			slope <- atan(slope) / (pi / 180)
			aspect <- aspect / (pi / 180)
		}
		
		layerNames(slope) <- 'slope'
		layerNames(aspect) <- 'aspect'
		x <- stack(slope, aspect)
	}

	if (filename != "") {
		x <- writeRaster(x, filename, ...)
	}
	return(x)
}

