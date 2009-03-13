# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : June 2008
# Version 0.8
# Licence GPL v3






rasterFromFile <- function(filename, values=FALSE, band=1) {
	warning("'rasterFromFile' is deprecated. Use 'raster(filename)' instead")

	fileext <- toupper(fileExtension(filename)) 
	if ( fileext == ".GRD" | fileext == ".GRI" ) {
		raster <- .rasterFromRasterFile(filename, band) 
	} else {
		raster <- .rasterFromGDAL(filename, band) 
	}
	if (values) {
		raster <- readAll(raster)
	}
	return(raster)
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
	function(x, ...) {
		return(setRaster(x))
	}
)


setMethod('raster', signature(x='character'), 
	function(x, values=FALSE, band=1) {
		return(rasterFromFile(x, values=values, band=band))
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
