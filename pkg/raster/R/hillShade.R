# Author: Andrew Bevan and Robert J. Hijmans
# Date : March 2010
# Version 1.0
# Licence GPL v3


hillShade <- function(slope, aspect, declination, direction, filename='', ...) {
	compare(slope, aspect)
	declination <- declination * pi/180
	direction <- direction * pi/180

	#x <- cos(slope) * cos(declination) + sin(slope) * sin(declination) * cos(direction-aspect)
	fun <- function(slp, asp) { cos(slp) * cos(declination) + sin(slp) * sin(declination) * cos(direction-asp) }
	x <- overlay(slope, aspect, fun=fun, filename=filename, ...)		
	return(x)
}

