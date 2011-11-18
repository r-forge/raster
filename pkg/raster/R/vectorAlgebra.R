
setMethod("+", signature(e1='SpatialPolygons', e2='SpatialPolygons'),
    function(e1, e2){ 
		require(rgeos)
		merge(e1, e2)
	}
)


setMethod("*", signature(e1='SpatialPolygons', e2='SpatialPolygons'),
    function(e1, e2){ 
		require(rgeos)
		crop(e1, e2)
	}
)


setMethod("-", signature(e1='SpatialPolygons', e2='SpatialPolygons'),
    function(e1, e2){ 
		require(rgeos)
		# needs more work to keep attributes
		gDifference(e1, e2, byid=TRUE)
	}
)


