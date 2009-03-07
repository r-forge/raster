# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3


.CanProcessInMemory <- function(raster, n=4) {
	gc()

	if (ncell(raster) > 2147483647) {
		return(FALSE) 
	}
	cells <- round(1.05 * ncell(raster))
	
	if (substr( R.Version()$platform, 1, 7) == "i386-pc" ) {
	# windows, function memory.size  available
		memneed <- cells * 8 * n / (1024 * 1024)
		if (memneed > memory.size(max = T)) {
			return(FALSE)
		} else {
			return(TRUE)
		}
	} else {
		w <- options('warn')[[1]]
		options('warn'=-1) 
		r <- try( matrix(NA, ncol=n, nrow=cells), silent=TRUE )
		options('warn'= w) 
		if (class(r) == "try-error") {
			return( FALSE )
		} else {
			return( TRUE ) 
		}
	}
}

