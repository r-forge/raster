

setAs('SpatialGridDataFrame', 'RasterStack',
	function(from){ return(asRasterStack (from)) }
)
setAs('SpatialPixelsDataFrame', 'RasterStack', 
	function(from){ return(asRasterStack (from)) }
)
	
setAs('SpatialGridDataFrame', 'RasterLayer', 
	function(from){ return(asRasterLayer (from)) }
)

setAs('SpatialGrid', 'RasterLayer', 
	function(from){ return(asRasterLayer (from)) }
)

setAs('SpatialPixelsDataFrame', 'RasterLayer', 
	function(from){ return(asRasterLayer (from)) }
)

setAs('SpatialPixels', 'RasterLayer', 
	function(from){ return(asRasterLayer (from)) }
)



setAs('RasterLayer', 'SpatialGridDataFrame', 
	function(from){ return(asSpGrid (from)) }
)

setAs('RasterStack', 'RasterLayer', 
	function(from){ return(asRasterLayer (from)) }
)




if (!isGeneric("asRasterLayer")) {
	setGeneric("asRasterLayer", function(object,index)
		standardGeneric("asRasterLayer"))
}	
setMethod('asRasterLayer', signature(object='missing',index='missing'), 
	function(object){
		return(raster())
	}
)
setMethod('asRasterLayer', signature(object='character',index='missing'), 
	function(object){
		r <- raster()
		if (object == 'runif') {
			r <- setValues(r, runif(ncell(r)))
		} else if (object == 'seq') {
			r <- setValues(r, 1:ncell(r))
		}
		return(r)
	}
)

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

setMethod('asRasterLayer', signature(object='RasterStack', index='numeric'), 
	function(object, index){
		if (nlayers(object) > 0) {
			dindex <- max(1, min(nlayers(object), index))
			if (dindex != index) { warning(paste("index was changed to", dindex))}
			rs <- object@layers[[dindex]]
			if (dataContent(object) == 'all') {
				rs <- setValues(rs, values(object)[,dindex])
			}
		} else {
			rs <- new("RasterLayer")
			rs <- setBbox(rs, getBbox(object))
			rs <- setRowCol(rs, nrow(object), ncol(object))
		}
		return(rs)
	}
)



setMethod('asRasterLayer', signature(object='SpatialPixels', index='numeric'), 
	function(object, index){
		r <- raster()
		r <- setBbox(r, getBbox(object))
		r <- setProjection(r, object@proj4string)
		r <- setRowCol(r, object@grid@cells.dim[2], object@grid@cells.dim[1])
		return(r)
	}
)

setMethod('asRasterLayer', signature(object='SpatialPixelsDataFrame', index='numeric'), 
	function(object, index){
		raster <- asRasterLayer(as(object, "SpatialPixels"))
		dindex <- max(1, min(dim(object@data)[2], index))
		if (dindex != index) { warning(paste("index was changed to", dindex))}
		sparse <- FALSE
		if (!sparse) {
			object <- as(object, 'SpatialGridDataFrame')
			raster <- setValues(raster, object@data[[dindex]])
		} else {
			cells <- object@grid.index
			if (length(cells)==0) {
				cells <- cellFromXY(raster, object@coords)
			}
			raster <- setValuesSparse(raster, cells, object@data[[dindex]])
		}
		return(raster)
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


asRasterStack <- function(spgrid) {
	stk <- new("RasterStack")
	stk <- setBbox(stk, getBbox(spgrid))
	stk <- setProjection(stk, spgrid@proj4string)
	stk <- setRowCol(stk, spgrid@grid@cells.dim[2], spgrid@grid@cells.dim[1])
	
	if (class(spgrid)=='SpatialPixels') {
		# do noting, there is no data
		# we could store the indices, but then we would have a sparse raster with no data (or with NAs). That goes against our definition of sparse (all NAs have been removed)
		stop('cannot make a RasterStack from a SpatialPixels object. Make a RasterLayer instead')		
	} else if (class(spgrid)=='SpatialPixelsDataFrame') {
		spgrid <- as(spgrid, 'SpatialGridDataFrame')
		rs <- as(stk, 'RasterLayer')
		stk <- setValues(stk, as.matrix(spgrid@data))
		for (i in 1:ncol(spgrid@data)) {
			stk@layers[i] <- rs
		}
	} else if ( class(spgrid)=='SpatialGrid' ) {
		# do nothing, there is no data
		stop('cannot make a RasterStack from a SpatialGrid object. Make a RasterLayer instead')
	} else if (class(spgrid)=='SpatialGridDataFrame' ) {
		stk <- setValues(stk, as.matrix(spgrid@data))
		rs <- as(stk, 'RasterLayer')
		stk <- setValues(stk, as.matrix(spgrid@data))
		for (i in 1:ncol(spgrid@data)) {
			stk@layers[i] <- rs
		}		
	}
	return(stk)
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
		data <- values(object)
		data <- as.data.frame(data)
		sp <- SpatialPixelsDataFrame(points=pts, data=data, proj4string=projection(object, FALSE)) 	
		
	} else if (type=='grid') {
		if ( dataContent(object) == 'all') {
			data <- values(object)
			data <- as.data.frame(data)
			sp <- SpatialGridDataFrame(grd, proj4string=projection(object, FALSE), data=data)
		} else { 
			sp  <- SpatialGrid(grd, proj4string=projection(object, FALSE))
		}	
	}
	return(sp)
}

