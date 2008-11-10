# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,1
# Licence GPL v3

s.calc <- function(rstack, fun, filename="", overwrite=FALSE, ForceIntOutput=FALSE) {
	if (length(fun(seq(1:5))) > 1) { stop("function 'fun' used returns more than one value") }

	outraster <- set.raster(rstack@rasters[[1]], filename)
	if (filename(outraster)=="") {
		rstack <- read.all(rstack)
		outraster <- set.values(outraster, apply(values(rstack), 1, fun)) 
	} else {
		if (ForceIntOutput) { outraster <- set.datatype(outraster, "integer") }
		for (r in 1:nrow(rstack)) {
			rstack <- read.row(rstack, r)
			vals <- apply(values(rstack), 1, fun)
			outraster <- set.values.row(outraster, vals, r) 
			outraster <- write.row(outraster, overwrite)
		}
	}		
	return(outraster)
}


#		res <- vector(mode = "numeric", length = rstack@ncols)

#		for (cl in 1:rstackset.raster(@ncols) {
#			celldata <- na.omit([cl,]) 
#			if (length(celldata) == 0) { res[cl] <- NA }
#			else { res[cl] <- fun(celldata) } 
#		}	
