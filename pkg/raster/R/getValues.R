# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3



if (!isGeneric("getValues")) {
	setGeneric("getValues", function(x, row, ...)
		standardGeneric("getValues"))
}	

setMethod("getValues", signature(x='Raster', row='missing'), 
function(x, format='', names=FALSE) {
	format <- trim(format)
	if (format=='') {
		format <- 'vector'
	}
	if (dataContent(x) != "all") {
		x <- readAll(x)
	}
	if (format=='matrix') { 
		return(.values.as.matrix(x, names)) 
	} else if (format=='dataframe') { 
		return(.values.as.dataframe(x)) 
	} else {
		return(x@data@values) 
	}
}
)


setMethod("getValues", signature(x='RasterStack', row='missing'), 
function(x, format='', names=FALSE) {
	if (format=='') {
		format <- 'matrix'
	}
	m <- matrix(nrow=ncell(x), ncol=nlayers(x))
	for (i in 1:nlayers(x)) {
		m[,i] <- getValues(raster(x, i))
	}
	if (names) {
		colnames(m) <- x@layernames
	} 
	if (format == 'dataframe') {
		m <- as.data.frame(m)
	}
	if (format == 'vector') {
		m <- as.vector(m)
	}
	return(m)
}
)

