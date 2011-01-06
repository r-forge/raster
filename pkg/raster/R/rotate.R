# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2009
# Version 0.9
# Licence GPL v3

	
if (!isGeneric("rotate")) {
	setGeneric("rotate", function(x, ...)
		standardGeneric("rotate"))
}	


setMethod('rotate', signature(x='Raster'), 
	function(x, ...) {
		e <- extent(x)
		xr <- e@xmax - e@xmin
		hx <- e@xmin + xr / 2
		r1 <- crop(x, extent(e@xmin, hx, e@ymin, e@ymax))
		r2 <- crop(x, extent(hx, e@xmax, e@ymin, e@ymax))
		r2@extent@xmin <- r2@extent@xmin - xr
		r2@extent@xmax <- r2@extent@xmax - xr
		m <- merge(r1, r2, ...)	
		m@layernames <- layerNames(x)
		return(m)
	}
)

