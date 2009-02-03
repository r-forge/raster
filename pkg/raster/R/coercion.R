# R code for changing rasters (spatial data)
# Authors: Robert J. Hijmans
# International Rice Research Institute
#contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.8
# Licence GPL v3




.asSpGrid <- function(object, type='grid', dataframe=TRUE)  {
	bb <- .toSpBbox(object)
	cs <- resolution(object)
	cc <- bb[,1] + (cs/2)
	cd <- ceiling(diff(t(bb))/cs)
	grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
	if (dataframe) {
		if (dataContent(object) != 'all') { 
			object <- readAll(object) 
		}
		data <- values(object)
		data <- as.data.frame(data)
	}
	if (type=='pixel') {
		object <- makeSparse(object)
		pts <- SpatialPoints(xyFromCell(object,  dataIndices(object)))
		if (dataframe) {
			sp <- SpatialPixelsDataFrame(points=pts, data=data, proj4string=projection(object, FALSE)) 	
		} else {
			sp <- SpatialPixels(points=pts, proj4string=projection(object, FALSE))
		}
	} else if (type=='grid') {
		if (dataframe) {
			sp <- SpatialGridDataFrame(grd, proj4string=projection(object, FALSE), data=data)
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
	function(from){ return(asRasterLayer (from)) }
)

	
setAs('SpatialGridDataFrame', 'RasterLayer', 
	function(from){ return(asRasterLayer (from)) }
)

setAs('SpatialPixelsDataFrame', 'RasterLayer', 
	function(from){ return(asRasterLayer (from)) }
)

setAs('SpatialGrid', 'RasterLayer', 
	function(from){ return(asRasterLayer (from)) }
)

setAs('SpatialPixels', 'RasterLayer', 
	function(from){ return(asRasterLayer (from)) }
)


setAs('SpatialGrid', 'RasterStack',
	function(from){ return(.asRasterStack (from)) }
)

setAs('SpatialGridDataFrame', 'RasterStack',
	function(from){ return(.asRasterStack (from)) }
)

setAs('SpatialPixels', 'RasterStack', 
	function(from){ return(.asRasterStack (from)) }
)

setAs('SpatialPixelsDataFrame', 'RasterStack', 
	function(from){ return(.asRasterStack (from)) }
)



if (!isGeneric("asRasterLayer")) {
	setGeneric("asRasterLayer", function(object, index)
		standardGeneric("asRasterLayer"))
}	


setMethod('asRasterLayer', signature(object='RasterStack'), 
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



setMethod('asRasterLayer', signature(object='SpatialPixelsDataFrame'), 
	function(object, index){
		r <- raster()
		r <- setBbox(r, getBbox(object))
		r <- setProjection(r, object@proj4string)
		r <- setRowCol(r, object@grid@cells.dim[2], object@grid@cells.dim[1])
		dindex <- max(1, min(dim(object@data)[2], index))
		if (dindex != index) { warning(paste("index was changed to", dindex))}
# to become an option, but currently support for sparse is too .....  sparse	
		sparse <- FALSE
		if (!sparse) {
				object <- as(object, 'SpatialGridDataFrame')
				r <- setValues(r, object@data[[dindex]])
		} else {
				cells <- object@grid.index
				if (length(cells)==0) {
					cells <- cellFromXY(r, object@coords)
				}
				r <- setValuesSparse(r, cells, object@data[[dindex]])
		}
		return(r)
	}
)



setMethod('asRasterLayer', signature(object='SpatialGridDataFrame'), 
	function(object, index){
		r <- raster()
		r <- setBbox(r, getBbox(object))
		r <- setProjection(r, object@proj4string)
		r <- setRowCol(r, object@grid@cells.dim[2], object@grid@cells.dim[1])
		dindex <- max(1, min(dim(object@data)[2], index))
		if (dindex != index) { warning(paste("index was changed to", dindex))}
		r <- setValues(r, object@data[[dindex]])
		return(r)
	}	
)




.asRasterStack <- function(spgrid) {
	stk <- new("RasterStack")
	stk <- setBbox(stk, getBbox(spgrid))
	stk <- setProjection(stk, spgrid@proj4string)
	stk <- setRowCol(stk, spgrid@grid@cells.dim[2], spgrid@grid@cells.dim[1])
	
	if (class(spgrid)=='SpatialPixelsDataFrame') {
		spgrid <- as(spgrid, 'SpatialGridDataFrame')
	}
	if (class(spgrid)=='SpatialGridDataFrame' ) {
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


