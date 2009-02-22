# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3


.CanProcessInMemory <- function(raster, n=2) {
	if (ncell(raster) > 2147483647) {
		return(FALSE) 
	}
	cells <- round(1.05 * ncell(raster))
	gc()
	w <- options('warn')
	options('warn'=-1) 
	r <- try( matrix(NA, ncol=n, nrow=cells), silent=TRUE )
	options('warn'=w[[1]]) 
	if (class(r) == "try-error") {
#	if (memneed > memory.size(max = T)) {
		return( FALSE )
	} else {
		return( TRUE ) 
	}
}

