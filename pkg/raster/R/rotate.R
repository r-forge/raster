# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2009
# Version 0.9
# Licence GPL v3

rotate <- function(raster) {
    xr <- xmax(raster) - xmin(raster)
	hx <- xr / 2
	r1 <- crop(raster, extent(xmin(raster), hx, ymin(raster), ymax(raster)))
	r2 <- crop(raster, extent(hx, xmax(raster), ymin(raster), ymax(raster)))
	xmax(r2) <- xmax(r2) - xr
	xmin(r2) <- xmin(r2) - xr
    m <- merge(r1, r2)	
    return(m)
}

