# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

if (!isGeneric("getValues")) {
	setGeneric("getValues", function(x, row, nrows, ...)
		standardGeneric("getValues"))
}	

setMethod("getValues", signature(x='RasterLayer', row='missing', nrows='missing'), 
function(x, format='', names=FALSE) {
	if (dataContent(x) != "all") {
		x <- readAll(x)
	}
	if (format=='matrix') { 
		return(.values.as.matrix(x, names=names)) 
	} else if (format=='dataframe') { 
		return(.values.as.dataframe(x)) 
	} else {
		return(x@data@values) 
	}
}
)

setMethod("getValues", signature(x='RasterBrick', row='missing', nrows='missing'), 
function(x) {
	if (dataContent(x) != "all") {
		x <- readAll(x)
	}
	colnames(x@data@values) <- layerNames(x)
	x@data@values
}
)


setMethod("getValues", signature(x='RasterStack', row='missing', nrows='missing'), 
function(x) {
	m <- matrix(nrow=ncell(x), ncol=nlayers(x))
	colnames(m) <- layerNames(x)
	for (i in 1:nlayers(x)) {
		m[,i] <- getValues(x@layers[[i]])
	}
	m
}
)

