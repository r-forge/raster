# Author: Robert J. Hijmans 
# International Rice Research Institute
# contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.8
# Licence GPL v3


setGeneric("cv", function(x, ..., aszero=FALSE, na.rm=FALSE)
	standardGeneric("cv"))

	
setMethod('cv', signature(x='ANY'), 
function(x, ..., aszero=FALSE, na.rm=FALSE) {
#  R function to compute the coefficient of variation (expressed as a percentage)
# if there is only a single value, sd = NA. However, one could argue that cv =0. 
# and NA may break the code that receives it.
#The function returns NA if(aszero=FALSE)   else a value of 0 is returned.
	x <- c(x, ...)
	z <- x[!is.na(x)]
	if (length(z) == 0) { 
		return(NA) 
	} else if (na.rm == FALSE & (length(z) < length(x))) { 
		return(NA)	 
	} else if (length(z) == 1 & aszero == TRUE) { 
		return(0)
	} else {
		x <- mean(z)
		if (x == 0) {
			return(NA)
		} else {
			return(100 * sd(z) / x)
		}
	}	
}
)


setMethod("cv", signature(x='Raster'),
	function(x, ..., aszero=FALSE, na.rm=FALSE){
		rasters <- list(...)
		if (class(x) == 'RasterLayer') {
			if (length(rasters)==0) { 
				return(x) 
			}
		}
		rasters <- c(x, rasters)
		rm(x)
		for (i in 1:length(rasters)) {
			if (class(rasters[[i]]) == 'RasterStack') {
				r <- rasters[[i]]
				rasters <- rasters[-i]
				rasters <- c(rasters, unstack(r))
				rm(r)
			}
		}
		fun <- function(x){modal(x, aszero=aszero)}
		return( .summaryRasters(rasters, fun, 'cv', na.rm=na.rm) )
	}
)


