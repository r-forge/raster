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


#setMethod('brick', signature(x='array'), 
#	function(x, xmn=0, xmx=1, ymn=0, ymx=1, projs=NA) {
#		r <- raster(ncols=ncol(x), nrows=nrow(x), projs=projs, xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx)
#		r <- setValues(r, as.vector(t(x)))
#		return(r)
#	}
#)



setMethod('brick', signature(x='Raster'), 
	function(x) {
		b <- brick(xmn=xmin(x), xmx=xmax(x), ymn=ymin(x), ymx=ymax(x), nrows=nrow(x), ncols=ncol(x), projs=projection(x))
		if (dataContent(x) == 'all') {
			b <- setValues(b, values(x))
		} 
		if (nlayers(x) == 1) {
			filename(b) <- trim(filename(x))
			if (filename(b) != '') {
				b@data@source == 'disk'	
			}
		}
		return(b)
	}
)


setMethod('brick', signature(x='RasterStack'), 
	function(x, index=1){
		if (nlayers(x) > 0) {
			b <- brick(x@layers[[1]])
			b@data@nlayers <- nlayers(x)
			if (dataContent(x) == 'all') {
				b <- setValues(b, values(x))
			}
		} else {
			b <- new("RasterBrick")
			extent(b) <- extent(x)
			rowcol(b) <- c(nrow(x), ncol(x))
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



setMethod('brick', signature(x='character'), 
	function(x, values=FALSE, proj=NULL, ...) {
		band <- 1
		fileext <- toupper(ext(x)) 
		if ( fileext == ".GRD" | fileext == ".GRI" | fileext == "" ) {
			if (fileext == "" & file.exists(x)) {
				r <- .rasterFromGDAL(x, band) 
			}
			grifile <- .setFileExtensionValues(x)
			grdfile <- .setFileExtensionHeader(x)
			if (file.exists( grdfile) ) {
				if (file.exists( grifile)) {
				    if (fileext != '.grd') { ext(x) <- '.grd' }
					r <- .rasterFromRasterFile(x, band) 
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
		} else if (file.exists( x )){
		    if (fileext == '.NC') {
				r <- .rasterCDF(x, ...) 
			} else {
				r <- .rasterFromGDAL(x, band) 
			}
		} else {
			stop(paste('file', x, 'does not exist'))
		}
		b <- brick(r)
		filename(b) <- filename(r)
		b@data@nlayers <- nbands(r)
		if (values) {
			b <- readAll(b)
		}
		b@data@source <- 'disk'
		if (!is.null(proj)) {
			projection(b) <- proj
		}
		return(b)
	}
)

