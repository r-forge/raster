# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : July 2011
# Version 1.0
# Licence GPL v3



if (!isGeneric("as.data.frame")) {
	setGeneric("as.data.frame", function(x, row.names = NULL, optional = FALSE, ...)
		standardGeneric("as.data.frame"))
}	


setMethod('as.data.frame', signature(x='Raster'), 
	function(x, row.names = NULL, optional = FALSE, ...) {
		if (.hasRAT(x)) {
			x <- ratToLayer(x)
		}
		v <- as.data.frame(values(x), row.names=row.names, optional=optional, ...)
		colnames(v) <- names(x)  # for nlayers = 1
		
		if (any(is.factor(x))) {
			f <- labels(x)
			for (i in 1:length(f)) {
				if (!is.null(f[[i]])) {
					v[,i] <- as.factor(f[[i]][v[,i]])
				}
			}
		}
		
		v
	}
)




