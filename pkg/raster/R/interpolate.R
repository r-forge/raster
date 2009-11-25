
if (!isGeneric("interpolate")) {
	setGeneric("interpolate", function(object, ...)
		standardGeneric("interpolate"))
}	


setMethod('interpolate', signature(object='Raster'), 
	function(object, model, filename="",  ...) {
		predict(object, model, filename, xy=TRUE, ...)
	}
)
