# R function for the raster package
# Author: Robert J. Hijmans
# International Rice Research Institute. Philippines
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.8
# Licence GPL v3



shift <- function(object, x=0, y=0) {
	object@bbox@xmin <- object@bbox@xmin + x
	object@bbox@ymin <- object@bbox@ymin + y
	object@bbox@xmax <- object@bbox@xmax + x
	object@bbox@ymax <- object@bbox@ymax + y
	return(object)
}
