# R raster package
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2008
# Version 0.9
# Licence GPL v3
	
if (!isGeneric("raster")) {
	setGeneric("raster", function(x, ...)
		standardGeneric("raster"))
}	


setMethod('raster', signature(x='missing'), 
	function(nrows=180, ncols=360, xmn=-180, xmx=180, ymn=-90, ymx=90, projs="+proj=longlat +datum=WGS84") {
		ext <- newExtent(xmn, xmx, ymn, ymx)
		r <- raster(ext, nrows=nrows, ncols=ncols, projs=projs)
		return(r)
	}
)

setMethod('raster', signature(x='matrix'), 
	function(x, xmn=0, xmx=1, ymn=0, ymx=1, projs=NA) {
		r <- raster(ncols=ncol(x), nrows=nrow(x), projs=projs, xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx)
		r <- setValues(r, as.vector(t(x)))
		return(r)
	}
)


setMethod('raster', signature(x='character'), 
	function(x, values=FALSE, band=1, proj=NULL, type='RasterLayer', ...) {
		fileext <- toupper(ext(x)) 
		if ( fileext == ".GRD" | fileext == ".GRI" | fileext == "" ) {
			if (fileext == "" & file.exists(x)) {
				r <- .rasterFromGDAL(x, band) 
			} else {
				grifile <- .setFileExtensionValues(x)
				grdfile <- .setFileExtensionHeader(x)
				if (file.exists( grdfile) ) {
					if (file.exists( grifile)) {
						if (fileext != '.grd') { ext(x) <- '.grd' }
						r <- .rasterFromRasterFile(x, band, type) 
					} else {
					# TODO check if this is a valid rater .grd but the problem is that the .gri is missing?
					
						if (fileext == ".GRD" ) {
							# check if this is a netcdf file
							fcon <- file(x, "rb")
							w <- readBin(fcon, what='character', n=1)
							close(fcon)
							if (substr(w, 1, 3) == "CDF") { 
								r <- .rasterCDF(x, ...) 
							} else {
							# perhaps a surfer grid...
								r <- .rasterFromGDAL(x, band) 
							}
						} else {
					# what would this be? A gri, but no grd. 
							stop('unknown file type; .gri file found but .grd is missing')
						}
					}
				} else {
					r <- .rasterFromGDAL(x, band) 
				}
			}
		} else if (file.exists( x )){
		    if (fileext == '.NC') {
				r <- .rasterCDF(x, ...) 
			} else {
				r <- .rasterFromGDAL(x, band) 
			}
		} else {
			stop(paste('file', x, 'does not exist'))
		}
		if (values) {
			r <- readAll(r)
		}
		if (!is.null(proj)) {
			projection(r) <- proj
		}
		return(r)
	}
)


setMethod('raster', signature(x='Raster'), 
	function(x, filename="", values=NULL) {
		r <- raster(xmn=xmin(x), xmx=xmax(x), ymn=ymin(x), ymx=ymax(x), nrows=nrow(x), ncols=ncol(x), projs=projection(x))
		filename(r) <- filename
		if (!is.null(values)) {
			x <- setValues(x, values)
		}
		return(r)
	}
)


setMethod('raster', signature(x='RasterStack'), 
	function(x, index=1){
		if (nlayers(x) > 0 & index > 0) {
			dindex <- max(1, min(nlayers(x), index))
			if (dindex != index) { warning(paste("index was changed to", dindex))}
			r <- x@layers[[dindex]]
		} else {
			r <- new("RasterLayer")
			extent(r) <- extent(x)
			rowcol(r) <- c(nrow(x), ncol(x))
		}
		return(r)
	}
)

setMethod('raster', signature(x='RasterBrick'), 
	function(x, index=1){
		if (nlayers(x) > 0) {
			dindex <- max(1, min(nlayers(x), index))
			if (filename(x) != '') {
				if (dindex != index) { warning(paste("index was changed to", dindex))}
				r <- raster(filename(x), band=dindex)
			} else {
				r <- raster(extent(x), nrows=nrow(x), ncols=ncol(x), projs=projection(x))	
				if (dataContent(x) == 'all') {
					if (dindex != index) { warning(paste("index was changed to", dindex))}
					r <- setValues(r, values(x)[,dindex])
				}
			}
		} else {
			r <- raster(extent(x), nrows=nrow(x), ncols=ncol(x), projs=projection(x))	
		}
		return(r)
	}
)



setMethod('raster', signature(x='Extent'), 
	function(x, nrows=10, ncols=10, projs=NA) {
		bb <- extent(x)
		nr = as.integer(round(nrows))
		nc = as.integer(round(ncols))
		if (nc < 1) { stop("ncols should be > 0") }
		if (nr < 1) { stop("nrows should be > 0") }
		r <- new("RasterLayer", extent=bb, ncols=nc, nrows=nr)
		projection(r) <- projs
		return(r) 
	}
)


setMethod('raster', signature(x='SpatialGrid'), 
	function(x, index=0){
		r <- raster()
		r <- setExtent(r, extent(x))
		projection(r) <- x@proj4string
		rowcol(r) <- c(x@grid@cells.dim[2], x@grid@cells.dim[1])		
		if (index > 0 & class(x) == 'SpatialGridDataFrame') {
			dindex <- max(1, min(dim(x@data)[2], index))
			if (dindex != index) { warning(paste("index was changed to", dindex))}
			r <- setValues(r, x@data[[dindex]])
		}
		return(r)
	}	
)


setMethod('raster', signature(x='SpatialPixels'), 
	function(x, index=0){
		r <- raster()
		r <- setExtent(r, extent(x))
		projection(r) <- x@proj4string
		rowcol(r) <- c(x@grid@cells.dim[2], x@grid@cells.dim[1])
		if (index > 0 & class(x) == 'SpatialPixelsDataFrame') {
			dindex <- max(1, min(dim(x@data)[2], index))
			if (dindex != index) { warning(paste("index was changed to", dindex))}
			x <- as(x, 'SpatialGridDataFrame')
			r <- setValues(r, x@data[[dindex]])
		}
		return(r)
	}
)



