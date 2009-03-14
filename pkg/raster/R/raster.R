# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : June 2008
# Version 0.8
# Licence GPL v3
	
if (!isGeneric("raster")) {
	setGeneric("raster", function(x, ...)
		standardGeneric("raster"))
}	


setMethod('raster', signature(x='missing'), 
	function(nrows=180, ncols=360, xmn=-180, xmx=180, ymn=-90, ymx=90, projs="+proj=longlat +datum=WGS84") {
		bb <- newBbox(xmn, xmx, ymn, ymx)
		r <- raster(bb, nrows=nrows, ncols=ncols, projs=projs)
		return(r)
	}
)


setMethod('raster', signature(x='character'), 
	function(x, values=FALSE, band=1) {
		fileext <- toupper(fileExtension(x)) 
		if ( fileext == ".GRD" | fileext == ".GRI" ) {
			r <- .rasterFromRasterFile(x, band) 
		} else {
			r <- .rasterFromGDAL(x, band) 
		}
		if (values) {
			r <- readAll(r)
		}
		return(r)
	}
)


setMethod('raster', signature(x='Raster'), 
	function(x, filename="", values=NULL) {
	
		if (class(x) == 'RasterStack') { 
			x <- asRasterLayer(x, 1) 
		}

		filename <- trim(filename)
		if (filename != "" & filename == filename(x)) {
			stop("it is not allowed to set the filename of the output RasterLayer to that of the input RasterLayer")
		}

		r <- raster(xmn=xmin(x), xmx=xmax(x), ymn=ymin(x), ymx=ymax(x), nrows=nrow(x), ncols=ncol(x), projs=projection(x))
		r <- setFilename(r, filename)
	
		if (!is.null(values)) {
			r <- setValues(r, values)
		}
		return(r)
	}
)



setMethod('raster', signature(x='BoundingBox'), 
	function(x, nrows=10, ncols=10, projs='NA') {
		bb <- getBbox(x)
		nr = as.integer(round(nrows))
		nc = as.integer(round(ncols))
		if (nc < 1) { stop("ncols should be > 0") }
		if (nr < 1) { stop("nrows should be > 0") }
		r <- new("RasterLayer", bbox=bb, ncols=nc, nrows=nr)
		r <- setProjection(r, projs)
		return(r) 
	}
)

