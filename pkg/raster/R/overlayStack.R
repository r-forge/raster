# Author: Robert J. Hijmans and Reinhard Krug
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3



setMethod('overlay', signature(x='RasterStack', y='missing'), 
function(x, y, fun, indices=1:nlayers(x), filename="", overwrite=FALSE, filetype='raster', datatype='FLT4S', track=-1){ 
	
	indices <- round(indices)
	if (min(indices) < 1) {	stop('indices should be >= 1') }
	if (max(indices) > nlayers(x)) {	stop('indices should be <= nlayers(x)') }
	
	rasters <- list()
	for (i in 1:length(indices)) {
		rasters[i] <- asRasterLayer(x, indices[i])
	}
	
	if (missing(fun)) { 
		stop("you must supply a function 'fun'. E.g., 'fun=function(x,y){return(x+y)}'") 
	}
	
	if (length(fun) == 1) {
		return(overlay(rasters, fun=fun, overwrite=overwrite, filetype=filetype, datatype=datatype, track=track))
	} else {
		if (filename != "" &&  (length(filename) != length(fun)) ) {
			stop('you must provide a filename for each function if you provide multiple functions')
		}
		
		# the idea is to optimize this, by reading all (row) data only once.... 
		res <- list()
		for (i in 1:length(fun)) {
			res[i] <- (overlay(rasters, fun=fun, overwrite=overwrite, filetype=filetype, datatype=datatype, track=track))
		}
		return(res)
	}
}
)

