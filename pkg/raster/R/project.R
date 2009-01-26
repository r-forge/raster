
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,1
# Licence GPL v3

.xyProj <- function(xy, inProj, outProj) {
	if (!isLatLon(inProj)) {
		xy <- project(xy, inProj, inv=TRUE)
	}
	xyproj <- project(xy, outProj, inv=FALSE)		
	return(xyproj)
}

projectRaster <- function(fromRaster, toRaster, method="nngb", overwrite=FALSE) {
# do the bounding boxes overlap at all? 
# get .innerbox first?
# are the projs not NA and valid and not the same?
warning("this function has not been tested yet. Not at all")
	rowCells <- 1:ncol(toRaster)
	inMemory <- filename(toRaster) == ""
	v <- vector(length=0)
	for (r in 1:nrow(toRaster)) {
		cells <- rowCells + (r-1) * ncol(toRaster)
		xy <- xyFromCell(toRaster, cells)
		projxy <- .xyProj(xy, projection(toRaster), projection(fromRaster))
		vals <- xyValues(fromRaster, projxy)
		if (inMemory) {
			v <- c(v, vals)
		} else {
			toRaster <- setValues(toRaster, vals, r)
			toRaster <- writeRaster(toRaster, overwrite=overwrite)
		}
	}
	if (inMemory) {
		toRaster <- setValues(toRaster, v) 
	}
	return(toRaster)
}

