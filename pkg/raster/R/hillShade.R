

hillShade <- function(slope, aspect, zenith, azimuth, filename='', ...) {
	zenith <- zenith * pi/180
	azimuth <- azimuth * pi/180
	x <- cos(slope) * cos(zenith) + sin(slope) * sin(zenith) * cos(azimuth-aspect)
	if (filename != "") {
		x <- writeRaster(x, filename, ...)
	}
	return(x)
}

