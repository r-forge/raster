# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3



setMethod('overlay', signature(x='RasterLayer', y='missing'), 
function(x, y, fun=sum, filename="", overwrite=FALSE, filetype='raster', datatype='FLT4S', track=-1){ 

	return(calc(x, fun=fun, filename=filename, overwrite=overwrite, filetype=filetype, datatype=datatype, track=track))
	
}
)


setMethod('overlay', signature(x='RasterLayer', y='RasterLayer'), 
function(x, y, ..., fun=sum, filename="", overwrite=FALSE, filetype='raster', datatype='FLT4S', track=-1){ 
	rasters <- c(x, y)
	obs <- list(...)
	if (isTRUE(length(obs) > 0)) {
		for (i in 1:length(obs)) {
			if (extends(class(obs[[i]]), "RasterLayer")) {
				rasters <- c(rasters, obs[[i]])
			} else {
				stop(paste("only RasterLayer objects allowed as ... arguments. Problem:", obs[[i]]))
			}
		}
	}
	return(.overlayList(rasters, fun=fun, filename=filename, overwrite=overwrite, filetype=filetype, datatype=datatype, track=track))
}
)


