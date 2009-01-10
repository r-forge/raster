# R code for changing rasters (spatial data)
# Authors: Robert J. Hijmans
# International Rice Research Institute
#contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.8
# Licence GPL v3



compare <- function(objects, bb=TRUE, rowcol=TRUE, prj=TRUE, res=FALSE, orig=FALSE, tolerance=0.05, stopiffalse=TRUE, showwarning=FALSE) {
	result <- TRUE
	if (!isTRUE(length(objects) > 1)) {
		result <- F
		stop('The first argument should consist of at least 2 Raster* objects')
	}	
	minres <- min(resolution(objects[[1]]))
	for (i in 2:length(objects)) { 
		if (bb) {
			if (!(isTRUE(all.equal(getBbox(objects[[1]]), getBbox(objects[[i]]), tolerance=tolerance, scale=minres )))) {
				result <- F
				if (stopiffalse) { stop('Different bounding box') }
				if (showwarning) { warning('Different bounding box') }
			}	
		}	
		if (rowcol) {
			if ( !(identical(ncol(objects[[1]]), ncol(objects[[i]]))) ) {
				result <- F
				if (stopiffalse) { stop('ncols different') } 
				if (showwarning) { warning('ncols different') } 
			}	
			if ( !(identical(nrow(objects[[1]]), nrow(objects[[i]]))) ) {
				result <- F
				if (stopiffalse) { stop('nrows different') }
				if (showwarning) { warning('nrows different') }
			}
		}
		if (prj) {
			if ( !(identical(projection(objects[[1]]), projection(objects[[i]]))))  {
				result <- F
				if (stopiffalse) {stop('different projection')}
				if (showwarning) { warning('different projection')}
			}
		}
# Can also check res through bb & rowcol
		if (res) {
			if (!(isTRUE(all.equal(resolution(objects[[1]]), resolution(objects[[i]]), tolerance=tolerance, scale=minres)))) {
				result <- F
				if (stopiffalse)  { stop('different resolution') }
				if (showwarning) { warning('different resolution') }
			}	
		}
# Can also check orig through bb & rowcol, but orig is useful for e.g. Merge(raster, raster)
		if (orig) {
			if (!(isTRUE(all.equal(origin(objects[[1]]), origin(objects[[i]]), tolerance=tolerance, scale=minres)))) {
				result <- F
				if (stopiffalse) { stop('different origin') }
				if (showwarning) { warning('different origin') }
			}	
		}
	}
	return(result)
}
