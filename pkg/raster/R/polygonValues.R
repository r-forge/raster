# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : December 2009
# Version 0.9
# Licence GPL v3



if (!isGeneric("polygonValues")) {
	setGeneric("polygonValues", function(p, r, ...)
		standardGeneric("polygonValues"))
}	


setMethod("polygonValues", signature(p='SpatialPolygons', r='Raster'), 
function(p, r, ...) {
	spbb <- bbox(p)
	rsbb <- bbox(r)
	if (spbb[1,1] >= rsbb[1,2] | spbb[1,2] <= rsbb[1,1] | spbb[2,1] >= rsbb[2,2] | spbb[2,2] <= rsbb[2,1]) {
		list()
	}
	npol <- length(p@polygons)
	res <- list()
	rr <- raster(r)
	for (i in 1:npol) {
		pp <- p[i,]
		spbb <- bbox(pp)
		
		if (spbb[1,1] >= rsbb[1,2] | spbb[1,2] <= rsbb[1,1] | spbb[2,1] >= rsbb[2,2] | spbb[2,2] <= rsbb[2,1]) {
			res[[i]] <- NULL
		} else {
			rc <- crop(rr, extent(pp))
			rc <- polygonsToRaster(pp, rc, silent=TRUE)
			xy <- rasterToPoints(rc)[,-3]
			if (length(xy) > 0)  {  # catch holes or very small polygons
				res[[i]] <- xyValues(r, xy)
			} else {
				res[[i]] <- NULL
			}
		}
	}
	res
}
)


