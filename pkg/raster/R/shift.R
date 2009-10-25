# R function for the raster package
# Author: Robert J. Hijmans
# International Rice Research Institute. Philippines
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.9
# Licence GPL v3



shift <- function(object, x=0, y=0) {
	object@extent@xmin <- object@extent@xmin + x
	object@extent@ymin <- object@extent@ymin + y
	object@extent@xmax <- object@extent@xmax + x
	object@extent@ymax <- object@extent@ymax + y
	return(object)
}
