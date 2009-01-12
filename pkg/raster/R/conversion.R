
setAs('RasterLayer', 'SpatialGridDataFrame', 
	function(from){ return(asSpGrid (from)) }
)

setAs('SpatialGridDataFrame', 'RasterBrick',
	function(from){ return(asRasterBrick (from)) }
)

setAs('SpatialGridDataFrame', 'RasterLayer', 
	function(from){ return(asRasterLayer (from)) }
)

setAs('RasterBrick', 'RasterLayer', 
	function(from){ return(asRasterLayer (from)) }
)

setAs('RasterStack', 'RasterLayer', 
	function(from){ return(asRasterLayer (from)) }
)




if (!isGeneric("asRasterLayer")) {
	setGeneric("asRasterLayer", function(object,index)
		standardGeneric("asRasterLayer"))
}	
setMethod('asRasterLayer', signature(object='Raster',index='missing'), 
	function(object){
		return(asRasterLayer(object, 1))
	}
)
setMethod('asRasterLayer', signature(object='SpatialPixels',index='missing'), 
	function(object){
		return(asRasterLayer(object, 1))
	}
)


setMethod('asRasterLayer', signature(object='RasterLayer', index='numeric'), 
	function(object, index){
		return(object)
	}
)

setMethod('asRasterLayer', signature(object='RasterStackBrick', index='numeric'), 
	function(object, index){
		rs <- newRaster(xmn = xmin(object), xmx = xmax(object), ymn = ymin(object), ymx = ymax(object), nrows=nrow(object), ncols=ncol(object), projstring=projection(object))
		if (dataContent(object) == 'all') {
			dindex <- max(1, min(nlayers(object), index))
			if (dindex != index) { warning(paste("index was changed to", dindex))}
			rs <- setValues(rs, as.matrix(values(object))[,dindex])
		}
		return(rs)
	}
)



setMethod('asRasterLayer', signature(object='SpatialPixels', index='numeric'), 
	function(object, index){
		raster <- newRaster()
		raster <- setBbox(raster, getBbox(object))
		raster <- setProjection(raster, object@proj4string)
		raster <- setRowCol(raster, object@grid@cells.dim[2], object@grid@cells.dim[1])
		return(raster)
	}
)

setMethod('asRasterLayer', signature(object='SpatialPixelsDataFrame', index='numeric'), 
	function(object, index){
		raster <- asRasterLayer(as(object, "SpatialPixels"))
		cells <- object@grid.index
		if (length(cells)==0) {
			cells <- cellFromXY(raster, object@coords)
		}
		dindex <- max(1, min(dim(object@data)[2], index))
		if (dindex != index) { warning(paste("index was changed to", dindex))}
		raster <- setValuesSparse(raster, cells, object@data[[dindex]])
	}
)	

setMethod('asRasterLayer', signature(object='SpatialGridDataFrame', index='numeric'), 
	function(object, index){
		raster <- asRasterLayer(as(object, "SpatialPixels"))
		dindex <- max(1, min(dim(object@data)[2], index))
		if (dindex != index) { warning(paste("index was changed to", dindex))}
		raster <- setValues(raster, object@data[[dindex]])
		return(raster)
	}	
)


asRasterBrick <- function(spgrid) {
	brick <- newBrick()
	brick <- setBbox(brick, getBbox(spgrid))
	brick <- setProjection(brick, spgrid@proj4string)
	brick <- setRowCol(brick, spgrid@grid@cells.dim[2], spgrid@grid@cells.dim[1])

	if (class(spgrid)=='SpatialPixels') {
		# do noting, there is no data
		# we could store the indices, but then we would have a sparse raster with no data (or with NAs). That goes against our definition of sparse (all NAs have been removed)
	} else if (class(spgrid)=='SpatialPixelsDataFrame') {
		cells <- spgrid@grid.index
		if (length(cells)==0) {
			cells <- cellFromXY(brick, spgrid@coords)
		}
		vals <- as.matrix(spgrid@data)
		brick <- setValuesSparse(brick, cells, vals)
	} else if ( class(spgrid)=='SpatialGrid' ) {
		# do nothing, there is no data
	} else if (class(spgrid)=='SpatialGridDataFrame' ) {
		brick <- setValues(brick, as.matrix(spgrid@data))
	}
	return(brick)
}


.toSpBbox <- function(object) {
	b <- getBbox(object)
	bb <- matrix(NA, 2, 2)
	bb[1,1] <- b@xmin
	bb[1,2] <- b@xmax
	bb[2,1] <- b@ymin
	bb[2,2] <- b@ymax
	return(bb)
}	



asSpGrid <- function(object, type='grid')  {
	bb <- .toSpBbox(object)
	cs <- resolution(object)
	cc <- bb[,1] + (cs/2)
	cd <- ceiling(diff(t(bb))/cs)
	grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
	if (type=='pixel') {
		object <- makeSparse(object)
		pts <- SpatialPoints(xyFromCell(object,  dataIndices(object)))
		sp <- SpatialPixelsDataFrame(points=pts, data=as.data.frame(values(object)), proj4string=projection(object, FALSE)) 	
		
	} else if (type=='grid') {
		if ( dataContent(object) == 'all') {
			values <- as.data.frame(values(object))
			sp <- SpatialGridDataFrame(grd, proj4string=projection(object, FALSE), data=values)
		} else { 
			sp  <- SpatialGrid(grd, proj4string=projection(object, FALSE))
		}	
	}
	return(sp)
}

