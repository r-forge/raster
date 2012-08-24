# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : August 2012
# Version 1.0
# Licence GPL v3


if (!isGeneric("layerize")) {
	setGeneric("layerize", function(x, ...)
		standardGeneric("layerize"))
}


setMethod('layerize', signature(x='RasterLayer'), 
	function(x, classes=NULL, digits=0, falseNA=FALSE, ...) {
		
		if (is.null(classes)) {
			classes <- round( sort(unique(x)), digits )
		}
				
		if (falseNA) {
			lyrs <- calc(x, function(x) {
					v <- round(x, digits) == classes
					v[v==0] <- NA
					v
				}
				, forceapply=TRUE, ...)
		} else {
			lyrs <- calc(x, function(x) round(x, digits) == classes, forceapply=TRUE, ...)
		}
		names(lyrs) <- as.character(classes)
		return(lyrs)
	}
)

