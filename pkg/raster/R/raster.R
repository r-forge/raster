# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : June 2008
# Version 0.8
# Licence GPL v3


rasterFromBbox <- function(bndbox, nrows=10, ncols=10) {
	warning("'rasterFromBbox' is deprecated. Use 'raster(bbox, ...)' instead")
	return(raster(bndbox=bndbox, nrows=nrows, ncols=ncols))
}

rasterFromFile <- function(filename, values=FALSE, band=1) {
	warning("'rasterFromFile' is deprecated. Use 'raster(filename)' instead")
	return(raster(filename, values=values, band=band))
}	
	
if (!isGeneric("raster")) {
	setGeneric("raster", function(x, ...)
		standardGeneric("raster"))
}	

setMethod('raster', signature(x='missing'), 
	function(nrows=180, ncols=360, xmn=-180, xmx=180, ymn=-90, ymx=90, projstring="+proj=longlat +datum=WGS84") {
		bb <- newBbox(xmn, xmx, ymn, ymx)
		rs <- raster(bb, nrows=nrows, ncols=ncols)
		rs <- setProjection(rs, projstring)
		return(rs)
	}
)

setMethod('raster', signature(x='Raster'), 
	function(x, filename="", values=NULL, layer=1) {
	
		if (class(x) == 'RasterStack') { 
			
			x <- asRasterLayer(x, layer) 
		}
		if (class(x) != 'RasterLayer') { stop('the first argument should be a RasterLayer or a RasterStack object') }

		filename <- trim(filename)
		if (filename != "" & filename == filename(x)) {
			stop("it is not allowed to set the filename of the output RasterLayer to that of the input RasterLayer")
		}

		r <- raster(xmn = xmin(x), xmx = xmax(x), ymn = ymin(x), ymx = ymax(x), nrows=nrow(x), ncols=ncol(x), projstring=projection(x))
		r <- setFilename(r, filename)
	
		if (!is.null(values)) {
			r <- setValues(r, values)
		}
		return(r)
	}
)

setMethod('raster', signature(x='character'), 
	function(x, values=FALSE, band=1) {
		fileext <- toupper(fileExtension(x)) 
		if ( fileext == ".GRD" | fileext == ".GRI" ) {
			raster <- .rasterFromRasterFile(x, band) 
		} else {
			raster <- .rasterFromGDAL(x, band) 
		}
		if (values) {
			raster <- readAll(raster)
		}
		return(raster)
	}
)



setMethod('raster', signature(x='BoundingBox'), 
function(x, nrows=10, ncols=10) {
	crs <- newCRS('NA')
	try(crs <- projection(x, asText=F), silent = T)
	
	bb <- getBbox(x)

	nr = as.integer(round(nrows))
	nc = as.integer(round(ncols))
	if (nc < 1) { stop("ncols should be > 0") }
	if (nr < 1) { stop("nrows should be > 0") }
	raster <- new("RasterLayer", bbox = bb, crs=crs, ncols = nc, nrows = nr )
	return(raster) 
}
)

