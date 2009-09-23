# Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


makeSparse <- function(object) {
	if ( class(object) == 'RasterStack' ) {
		for (i in 1:nlayers(object)) {
			object@layers[[i]] <- .makeSparse(object@layers[[i]])
		}
	} else {
		object <- .makeSparse(object)	
	}
	return(object)
}


.makeSparse <- function(raster) {
	if (dataContent(raster) == 'sparse') { return(raster) }
	vals <- seq(1:ncell(raster))
	vals <- cbind(vals, values(raster))
	vals <- na.omit(vals)
	raster <- setValuesSparse(raster, sparsevalues=vals[,2], cellnumbers=vals[,1])
	return(raster)
}
