# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3



setMethod('overlay', signature(x='RasterLayer', y='missing'), 
function(x, y, fun=sum, filename="", ...){ 

	return(calc(x, fun=fun, filename=filename, ...))
	
}
)


setMethod('overlay', signature(x='RasterLayer', y='RasterLayer'), 
function(x, y, ..., fun=sum, filename="", datatype, filetype, overwrite, track){ 
	if (missing(datatype)) {
		datatype <- .datatype(datatype)
	}
	if (missing(filetype)) {
		filetype <- .filetype(...)
	} 
	if (missing(overwrite)) {
		overwrite <- .overwrite(...)
	}
	if (missing(track)) {
		track <- .track(...)
	}

	rasters <- c(x, y)
	obs <- list(...)
	if (isTRUE(length(obs) > 0)) {
		for (i in 1:length(obs)) {
			if (extends(class(obs[[i]]), "RasterLayer")) {
				rasters <- c(rasters, obs[[i]])
			}
		}
	}
	
	return(.overlayList(rasters, fun=fun, filename=filename, datatype=datatype, filetype=filetype, overwrite=overwrite, track=track))
}
)


