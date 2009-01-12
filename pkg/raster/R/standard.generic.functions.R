# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,8
# Licence GPL v3


setMethod('dim', signature(x='BasicRaster'), 
	function(x){ return(c(nrow(x), ncol(x), nlayers(x)))}
)

setMethod('nrow', signature(x='BasicRaster'), 
	function(x){ return(x@nrows)}
)

setMethod('ncol', signature(x='BasicRaster'), 
	function(x){ return(x@ncols) }
)



setMethod('summary', signature(object='RasterStackBrick'), 
	function(object, ...) {
		if (dataContent(object) == 'all') {
			for (n in 1:nlayers(object)) {
				cat("layer ", n, "\n")
				cat("NAs  : ", sum(is.na(values(object)[,n])), "\n")
				summary(values(object)[,n])
			}
		} else {
			cat("values not in memory\n")
		}
	}
)	

setMethod('summary', signature(object='RasterLayer'), 
	function(object, ...) {
		cat ("Cells: " , ncells(object), '\n')
		if ( dataContent(object) == "all") {
			cat("NAs  : ", sum(is.na(values(object))), "\n")
			summary(values(object))
		} else {
			cat("values not in memory\n")
		}
	}	
)

