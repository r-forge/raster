# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,1
# Licence GPL v3

s.calc <- function(rstack, fun, filename="", overwrite=FALSE, ForceIntOutput=FALSE) {
	if (length(fun(seq(1:5))) > 1) { stop("function 'fun' used returns more than one value") }

	outraster <- setRaster(rstack@rasters[[1]], filename)
	if (filename(outraster)=="") {
		rstack <- .rasterstack.read.all(rstack)
		outraster <- setValues(outraster, apply(values(rstack), 1, fun)) 
	} else {
		if (ForceIntOutput) { outraster <- setDatatype(outraster, "integer") }
		for (r in 1:nrow(rstack)) {
			rstack <- readRow(rstack, r)
			vals <- apply(values(rstack), 1, fun)
			outraster <- setValuesRow(outraster, vals, r) 
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
