# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.9
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


#setMethod('length', signature(x='Raster'),  function(x) { return(length(x@data@values)) )

