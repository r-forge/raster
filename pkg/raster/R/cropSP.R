# Author: Robert J. Hijmans
# Date : November 2011
# Version 1.0
# Licence GPL v3



setMethod('crop', signature(x='SpatialPolygons', y='ANY'), 
function(x, y, ...) {
	require(rgeos)

	if (! inherits(y, 'SpatialPolygons')) {
		if (inherits(y, 'Extent')) {
			y <- as(y, 'SpatialPolygons')
		} else { 
			y <- extent(y)
			validObject(y)
			y <- as(y, 'SpatialPolygons')
		}
		y@proj4string <- x@proj4string		
	}
	
	if (.hasSlot(x, 'data')) {
	
		# in future versions of rgeos, this intermediate step should not be necessary
		i <- as.vector( gIntersects(x, y, byid=TRUE) )
		if (sum(i) == 0) {
			return(NULL)
		}
		y <- gIntersection(x[i,], y, byid=TRUE)
		ids <- sapply(y@polygons, function(x) strsplit(slot(x, 'ID'), ' '))
		ids <- as.numeric(do.call(rbind, ids)[,1])
		for (i in 1:length(y@polygons)) {
			y@polygons[[i]]@ID <- as.character(ids[i])
		}
		data <- x@data[ids, ,drop=FALSE]
		rownames(data) <- ids
		SpatialPolygonsDataFrame(y, data)
	} else {
		gIntersection(x, y)
	}
}
)



setMethod('crop', signature(x='SpatialLines', y='ANY'), 
function(x, y, ...) {
	require(rgeos)

	if (! inherits(y, 'SpatialPolygons')) {
		if (inherits(y, 'Extent')) {
			y <- as(y, 'SpatialPolygons')
		} else { 
			y <- as(extent(y), 'SpatialPolygons')
		}
		y@proj4string <- x@proj4string		
	}
	
	if (.hasSlot(x, 'data')) {
	
		# in future versions of rgeos, this intermediate step should not be necessary
		i <- as.vector( gIntersects(x, y, byid=TRUE) )
		if (sum(i) == 0) {
			return(NULL)
		}
		y <- gIntersection(x[i,], y, byid=TRUE)
		ids <- sapply(y@lines, function(x) strsplit(slot(x, 'ID'), ' '))
		ids <- as.numeric(do.call(rbind, ids)[,1])
		for (i in 1:length(y@lines)) {
			y@lines[[i]]@ID <- as.character(ids[i])
		}
		data <- x@data[ids, ,drop=FALSE]
		rownames(data) <- ids
		SpatialLinesDataFrame(y, data)
	} else {
		gIntersection(x, y)
	}
}
)

