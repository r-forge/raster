
asRaster <- function(spgrid, getdata=TRUE, dataindex=1) {
	raster <- newRaster()
	raster@bbox <- spgrid@bbox
	raster@proj4string <- spgrid@proj4string
	raster@ncols <- spgrid@grid@cells.dim[1]
	raster@nrows <- spgrid@grid@cells.dim[2]
	if (getdata) {
		if (class(spgrid)=='SpatialPixels') {
			# do noting, there is no data
			# we could store the indices, but then we would have a sparse raster with no data (or with NAs). That goes against our definition of sparse (all NAs have been removed)
		} else if (class(spgrid)=='SpatialPixelsDataFrame') {
			cells <- spgrid@grid.index
			if (length(cells)==0) {
				cells <- cellFromXY(raster, spgrid@coords)
			}
			vals <- spgrid@data[[dataindex]]
			raster <- setValuesSparse(raster, cells, vals)
		} else if ( class(spgrid)=='SpatialGrid' ) {
			# do nothing, there is no data
		} else if (class(spgrid)=='SpatialGridDataFrame' ) {
			raster <- setValues(raster, spgrid@data[[dataindex]])
		}
	}
	return(raster)
}


asBrick <- function(spgrid, getdata=TRUE) {
	brick <- newBrick()
	brick@bbox <- spgrid@bbox
	brick@proj4string <- spgrid@proj4string
	brick@ncols <- spgrid@grid@cells.dim[1]
	brick@nrows <- spgrid@grid@cells.dim[2]
	if (getdata) {
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
	}
	return(brick)
}


asSpgrid <- function(raster, type='grid')  {
	bb <- boundingbox(raster)
	cs <- resolution(raster)
	cc <- bb[,1] + (cs/2)
	cd <- ceiling(diff(t(bb))/cs)
	grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
	
	if (type=='pixel') {
		raster <- makeSparse(raster)
		pts <- SpatialPoints(xyFromCell(raster,  dataIndices(raster)))
		sp <- SpatialPixelsDataFrame(points=pts, data=as.data.frame(values(raster)), proj4string=projection(raster, FALSE)) 	
		
	} else if (type=='grid') {
		if ( dataContent(raster) == 'all') {
			sp <- SpatialGridDataFrame(grd, proj4string=projection(raster, FALSE), data=as.data.frame(values(raster)))
		} else { 
			sp  <- SpatialGrid(grd, proj4string=projection(raster, FALSE))
		}	
	}
	return(sp)
}

