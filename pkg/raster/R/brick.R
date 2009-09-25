# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  September 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric("brick")) {
	setGeneric("brick", function(x, ...)
		standardGeneric("brick"))
}	



setMethod('brick', signature(x='missing'), 
	function(nrows=180, ncols=360, xmn=-180, xmx=180, ymn=-90, ymx=90, projs="+proj=longlat +datum=WGS84") {
		extent <- newExtent(xmn, xmx, ymn, ymx)
		b <- brick(extent, nrows=nrows, ncols=ncols, projs=projs)
		return(b)
	}
)



setMethod('brick', signature(x='character'), 
	function(x, values=FALSE, proj=NULL, ...) {
		b <- raster(x, type='RasterBrick')
		if (class(b) == 'RasterLayer') {
			b <- brick(b)
		}
		if (values) {
			b <- readAll(b)
		}
		if (!is.null(proj)) {
			projection(b) <- proj
		}
		return(b)
	}
)


setMethod('brick', signature(x='Raster'), 
	function(x) {
		b <- brick(xmn=xmin(x), xmx=xmax(x), ymn=ymin(x), ymx=ymax(x), nrows=nrow(x), ncols=ncol(x), projs=projection(x))
		b@file <- x@file
		b@data@nlayers <- nbands(x)
		f <- trim(filename(x))
		if (f != '') {
			b@data@source <- 'disk'
		} else {
			b@data@nlayers <- as.integer(1)
			if (dataContent(x) == 'all') {
				b <- setValues(b, as.matrix(values(x)), rownr=-1, layer=1)
			}
		}
		return(b)
	}
)


setMethod('brick', signature(x='RasterStack'), 
	function(x){
		b <- brick(extent(x), nrows=nrow(x), ncols=ncol(x), projs=projection(x))
		if (nlayers(x) > 0) {
			for (i in 1:nlayers(x)) {
				b <- addLayer(b, raster(x, i))
			}
		}
		return(b)
	}
)




setMethod('brick', signature(x='Extent'), 
	function(x, nrows=10, ncols=10, projs=NA) {
		bb <- extent(x)
		nr = as.integer(round(nrows))
		nc = as.integer(round(ncols))
		if (nc < 1) { stop("ncols should be > 0") }
		if (nr < 1) { stop("nrows should be > 0") }
		b <- new("RasterBrick", extent=bb, ncols=nc, nrows=nr)
		projection(b) <- projs
		return(b) 
	}
)


setMethod('brick', signature(x='SpatialGrid'), 
	function(x, index=0){
		b <- brick()
		extent(b) <- extent(x)
		projection(b) <- x@proj4string
		rowcol(b) <- c(x@grid@cells.dim[2], x@grid@cells.dim[1])		
		if (class(x) == 'SpatialGridDataFrame') {
			b <- setValues(r, x@data)
		}
		return(b)
	}	
)


setMethod('brick', signature(x='SpatialPixels'), 
	function(x, index=0){
		b <- brick()
		exent(b) <- extent(x)
		projection(b) <- x@proj4string
		rowcol(b) <- c(x@grid@cells.dim[2], x@grid@cells.dim[1])
		if (class(x) == 'SpatialPixelsDataFrame') {
			x <- as(x, 'SpatialGridDataFrame')
			b <- setValues(b, x@data)
		}
		return(b)
	}
)


