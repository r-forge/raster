# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3


.asSpGrid <- function(object, type='grid', dataframe=TRUE)  {
	bb <- .toSpBbox(object)
	cs <- res(object)
	cc <- bb[,1] + (cs/2)
	cd <- ceiling(diff(t(bb))/cs)
	grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
	if (dataframe) {
		vals <- getValues(object)
		vals <- as.data.frame(vals)
	}
	if (type=='pixel') {
		vals <- na.omit( cbind(1:ncell(object), getValues(object)) )
		pts <- SpatialPoints(xyFromCell(object,  vals[,1]))
		if (dataframe) {
			sp <- SpatialPixelsDataFrame(points=pts, data=vals[,2], proj4string=projection(object, FALSE)) 	
		} else {
			sp <- SpatialPixels(points=pts, proj4string=projection(object, FALSE))
		}
	} else if (type=='grid') {
		if (dataframe) {
			sp <- SpatialGridDataFrame(grd, proj4string=projection(object, FALSE), data=vals)
		} else { 
			sp  <- SpatialGrid(grd, proj4string=projection(object, FALSE))
		}	
	}
	return(sp)
}

setAs('RasterLayer', 'SpatialPixels', 
	function(from) { return(.asSpGrid(from, type='pixel', FALSE)) }
)

setAs('RasterLayer', 'SpatialPixelsDataFrame', 
	function(from) { return(.asSpGrid(from, type='pixel', TRUE)) }
)

setAs('RasterLayer', 'SpatialGrid', 
	function(from) { return(.asSpGrid(from, type='grid', FALSE)) }
)

setAs('RasterLayer', 'SpatialGridDataFrame', 
	function(from) { return(.asSpGrid(from, type='grid', TRUE)) }
)


setAs('RasterStack', 'SpatialGridDataFrame', 
	function(from) { return(.asSpGrid(from, type='grid', TRUE)) }
)


setAs('RasterStack', 'RasterLayer', 
	function(from){ return( raster (from)) }
)

	
setAs('SpatialGridDataFrame', 'RasterLayer', 
	function(from){ return( raster (from)) }
)

setAs('SpatialPixelsDataFrame', 'RasterLayer', 
	function(from){ return(raster (from)) }
)

setAs('SpatialGrid', 'RasterLayer', 
	function(from){ return(raster (from)) }
)

setAs('SpatialPixels', 'RasterLayer', 
	function(from){ return(raster (from)) }
)


setAs('SpatialGrid', 'RasterStack',
	function(from){ return(stack(from)) }
)

setAs('SpatialGridDataFrame', 'RasterStack',
	function(from){ return(stack(from)) }
)

setAs('SpatialPixels', 'RasterStack', 
	function(from){ return(stack(from)) }
)

setAs('SpatialPixelsDataFrame', 'RasterStack', 
	function(from){ return(stack(from)) }
)




.toSpBbox <- function(object) {
	b <- extent(object)
	bb <- matrix(NA, 2, 2)
	bb[1,1] <- b@xmin
	bb[1,2] <- b@xmax
	bb[2,1] <- b@ymin
	bb[2,2] <- b@ymax
	return(bb)
}	



setAs('matrix', 'RasterLayer',
	function(from){ return(raster(from)) }
)

setAs('RasterLayer', 'matrix',
	function(from){ return( values(from, format='matrix')) }
)
