# Author: Robert J. Hijmans
# Date : November 2011
# Version 1.0
# Licence GPL v3


setMethod("+", signature(e1='Spatial', e2='Spatial'),
    function(e1, e2){ 
		join(e1, e2)
	}
)

setMethod("*", signature(e1='SpatialPolygons', e2='SpatialPolygons'),
    function(e1, e2){ 
		intersect(e1, e2)	}
)

setMethod("-", signature(e1='SpatialPolygons', e2='SpatialPolygons'),
    function(e1, e2){ 
		erase(e1, e2)
	}
)

#setMethod("^", signature(e1='SpatialPolygons', e2='SpatialPolygons'),
#    function(e1, e2){ 
#		crop(e1, e2)
#	}
#)
