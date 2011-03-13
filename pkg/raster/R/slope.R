# Author: Robert J. Hijmans
# Date : February 2010
# Version 1.0
# Licence GPL v3


slopeAspect <- function(alt, filename='', type='both', unit='', ...) {
	
	type <- trim(tolower(type))
	stopifnot(type %in% c('', 'both' , 'slope', 'aspect'))
	unit <- trim(tolower(unit))
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
	
		fX <- fX / (8 * xres)
		fY <- fY / (8 * yres)
		zx <- focalFilter(alt, fX)
		zy <- focalFilter(alt, fY)
	}

	if (type == 'slope') {
		
		x <- sqrt( zy^2 + zx^2 ) 
		if (unit == 'degrees') {
			x <- atan(x) * (180 / pi)
		}
		layerNames(x) <- 'slope'
		
	} else if (type == 'aspect') {
		x <- atan2(zy, zx)
		x <- ((0.5*pi)-x) %% (2*pi)
		if (unit == 'degrees') {
			x <- x * (180/pi)
		}
		layerNames(x) <- 'aspect'
		
	} else {
		x <- sqrt( zy^2 + zx^2 ) 
		aspect <- atan2(zy, zx) 
		aspect <- ((0.5*pi)-aspect) %% (2*pi)
		
		if (unit == 'degrees') {
			x <- atan(x) * (180/pi)
			aspect <- aspect * (180/pi)
		}
		
		layerNames(x) <- 'slope'
		layerNames(aspect) <- 'aspect'
		x <- stack(x, aspect)
	}

	if (filename != "") {
		x <- writeRaster(x, filename, ...)
	}
	return(x)
}

