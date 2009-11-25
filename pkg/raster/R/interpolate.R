
if (!isGeneric("interpolate")) {
	setGeneric("interpolate", function(object, ...)
		standardGeneric("interpolate"))
}	


setMethod('interpolate', signature(object='Raster'), 
	function(object, model, filename="", newdata=NULL, ...) {
		if (is.null(newdata)) { 
			object <- raster(object)
		} else { 
			object <- newdata 
		}
		predict(object, model, filename, xy=TRUE, ...)
	}
)
