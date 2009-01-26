# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  Jaunary 2009
# Version 0.8
# Licence GPL v3

resample <- function(from, to, method="nngb", overwrite=FALSE) {
# do the bounding boxes overlap at all? 
# get .innerbox first?
	rowCells <- 1:ncol(to)
	inMemory <- filename(to) == ""
	v <- vector(length=0)
	for (r in 1:nrow(to)) {
		cells <- rowCells + (r-1) * ncol(to)
		xy <- xyFromCell(to, cells)
		vals <- xyValues(from, xy)
		if (inMemory) {
			v <- c(v, vals)
		} else {
			to <- setValues(to, vals, r)
			to <- writeRaster(to, overwrite=overwrite)
		}
	}
	if (inMemory) {
		to <- setValues(to, v) 
	}
	return(to)
}



