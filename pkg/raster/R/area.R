# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2008
# Version 0.9
# Licence GPL v3

area <- function(x, filename='', ...) {
	if (isLatLon(x) | projection(x)=='NA') {
		out <- raster(x)
		filename <- trim(filename)
		if (!canProcessInMemory(x, 3) & filename == '') {
			filename <- rasterTmpFile()
		}
		
		if (filename == '') {
			v <- matrix(NA, ncol=nrow(out), nrow=ncol(out))
		} else {
			v <- vector(length=ncol(out))
		}

		ry <- yres(out)
		rx <- xres(out)		
		dy <- pointDistance(c(0,0),c(0,ry),'GreatCircle')

		pb <- pbCreate(nrow(out), type=.progress(...))
		for (r in 1:nrow(out)) {
			y <- yFromRow(out, r)
			dx <- pointDistance(c(0,y),c(rx,y),'GreatCircle')
			if (filename == "") {
				v[,r] <- dx * dy / 1000000
			} else {
				v[] <- dx * dy / 1000000
				out <- setValues(out, v, r)
				out <- writeRaster(out)
			}
			pbStep(pb, r)
		}
		pbClose(pb)
		if (filename == "") { 
			out <- setValues(out, as.vector(v))
		}
		return(out)
	} else {
		stop('This function is only (useful) for layer with a lat/lon CRS')
	}
}
