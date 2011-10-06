# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric("distance")) {
	setGeneric("distance", function(x, ...)
		standardGeneric("distance"))
}	


setMethod('distance', signature(x='RasterLayer'), 
function(x, filename='', doEdge=FALSE, ...) {
	if (doEdge) {
		r <- edge(x, classes=FALSE, type='inner', asNA=TRUE, progress=.progress(...)) 
		pts <- try(  rasterToPoints(r, fun=function(z){ z>0 } )[,1:2, drop=FALSE] )
	} else {
		pts <- try(  rasterToPoints(x, fun=function(z){ z>0 } )[,1:2, drop=FALSE] )
	}
	
	if (class(pts) == "try-error") {
		return( .distanceRows(x, filename=filename, ...) )
	}
	if (nrow(pts) == 0) {
		stop('RasterLayer has no NA cells (for which to compute a distance)')
	}
	out <- raster(x)
	distanceFromPoints(out, pts, filename=filename, ...)
}
)
