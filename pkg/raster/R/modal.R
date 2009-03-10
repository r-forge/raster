# Author: Robert J. Hijmans 
# International Rice Research Institute
# contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.8
# Licence GPL v3


setGeneric("modal", function(x, ..., ties='random', na.rm=FALSE)
	standardGeneric("modal"))
	
	
setMethod('modal', signature(x='ANY'), 
function(x, ..., ties='random', na.rm=FALSE) {
#partly based on http://wiki.r-project.org/rwiki/doku.php?id=tips:stats-basic:modalvalue
	if (!ties %in% c('lowest', 'highest', 'NA', 'random')) {
		stop("ties should be: 'lowest', 'highest', 'NA', or 'random'")
	}

	x <- c(x, ...)
	
	z <- x[!is.na(x)]
	if (length(z) == 0) { return(NA) 
	} else if (na.rm == FALSE & length(z) < length(x)) { 
		return(NA)	 
	} else if (length(z) == 1) {
		return(z)
	} else {
		freq <- table(z)
		w <- as.numeric(names(freq[max(freq)==freq]))
		if (length(w) > 1) {
			if (ties == 'lowest') {
				w <- min(w)
			} else if (ties == 'highest') {
				w <- max(w)
			} else if (ties == 'NA') {
				w <- NA
			} else { # random
				r <- runif(length(w))
				w <- w[which.max(r)]
			}	
		} 
		return(w)
	}	
}
)




setMethod("modal", signature(x='Raster'),
	function(x, ..., ties='random', na.rm=FALSE){
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
		fun <- function(x){modal(x, ties=ties)}
		return( .summaryRasters(rasters, fun, 'modal', na.rm=na.rm) )
	}
)

