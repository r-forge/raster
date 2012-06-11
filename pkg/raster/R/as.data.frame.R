# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : July 2011
# Version 1.0
# Licence GPL v3



if (!isGeneric("as.data.frame")) {
	setGeneric("as.data.frame", function(x, row.names = NULL, optional = FALSE, ...)
		standardGeneric("as.data.frame"))
}	


.insertColsInDF <- function(x, y, col) {
	if (ncol(y) == 1) {
		x[, col] <- y
	} else if (col==1) {
		data.frame(y, x[, -1, drop=FALSE])
	} else {
		cbind(x[,-col, drop=FALSE], y)
	}
}

setMethod('as.data.frame', signature(x='Raster'), 
	function(x, row.names = NULL, optional = FALSE, ...) {

		v <- as.data.frame(values(x), row.names=row.names, optional=optional, ...)
	
		colnames(v) <- names(x)  # for nlayers = 1
		
		i <- is.factor(x)
		if (any(i)) {
			i <- which(i)
			for (j in i) {
				fv <- factorValues(x, v[,j], j)
				colnames(fv) <- paste(names(x)[j], '_', colnames(fv), sep='')
				v <- .insertColsInDF(v, fv, j)
			}
		}
	
		v
	}
)




