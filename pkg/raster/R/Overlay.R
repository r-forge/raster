# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,1
# Licence GPL v3


Overlay <- function(raster1, raster2, ..., fun=function(x,y){return(x+y)}, filename="", overwrite=FALSE) {
#	if (class(raster1) != 'RasterLayer' | class(raster2) != 'RasterLayer') {
#		stop('first two arguments should be objects of class "RasterLayer"')}
	rasters <- c(raster1, raster2)
	obs <- list(...)
	if (isTRUE(length(obs) > 0)) {
		for (i in 1:length(obs)) {
			if (extends(class(obs[[i]]), "RasterLayer")) {
				rasters <- c(rasters, obs[[i]])
			} 
		}
	}
	if (length(rasters) > 6) {stop("sorry, this function cannot take more than 5 RasterLayers at a time")}
	
	for (i in 2:length(rasters)) {
		if (!compare(c(raster1, rasters[i]))) { 
			stop('Extent and/or resolution of rasters do not match') 
		}	
	}
	outraster <- setRaster(raster1)
	outraster <- setFilename(outraster, filename)

	inram <- TRUE
	ondisk <- TRUE
	for (i in 1:length(rasters)) {
		if (dataContent(rasters[[i]]) != 'all') {inram <- FALSE} 
		if (dataSource(rasters[[i]]) != 'disk') {ondisk <- FALSE} 		
	}	
		

	if ( inram ) {
	# there has to be a smarter way then this!
		if (length(rasters) == 2) {
			vals <- fun( values(rasters[[1]]), values(rasters[[2]]) )
		} else if (length(rasters) == 3) {
			vals <- fun( values(rasters[[1]]), values(rasters[[2]]), values(rasters[[3]]) )
		} else if (length(rasters) == 4) {
			vals <- fun( values(rasters[[1]]), values(rasters[[2]]), values(rasters[[3]]), values(rasters[[4]]) )
		} else if (length(rasters) == 5) {
			vals <- fun( values(rasters[[1]]), values(rasters[[2]]), values(rasters[[3]]), values(rasters[[4]]), values(rasters[[5]]) )
		} else if (length(rasters) == 6) {
			vals <- fun( values(rasters[[1]]), values(rasters[[2]]), values(rasters[[3]]), values(rasters[[4]]), values(rasters[[5]]), values(rasters[[6]]) )
		}
		
		outraster <- setValues(outraster, vals)
		if (filename(outraster) != "") { writeRaster(outraster, overwrite=overwrite) }
	} else if ( ondisk ) {
		v <- vector(length=0)
		for (r in 1:nrow(outraster)) {
			for (i in 1:length(rasters)) {
				if (dataSource(rasters[[i]]) == 'ram') {
					rasters[i] <- valuesRow(rasters[[i]], r)
				} else {	
					rasters[i] <- readRow(rasters[[i]], r)
				}	
			}	
			if (length(rasters) == 2) {
				vals <- fun( values(rasters[[1]]), values(rasters[[2]]) )
			} else if (length(rasters) == 3) {
				vals <- fun( values(rasters[[1]]), values(rasters[[2]]), values(rasters[[3]]) )
			} else if (length(rasters) == 4) {
				vals <- fun( values(rasters[[1]]), values(rasters[[2]]), values(rasters[[3]]), values(rasters[[4]]) )
			} else if (length(rasters) == 5) {
				vals <- fun( values(rasters[[1]]), values(rasters[[2]]), values(rasters[[3]]), values(rasters[[4]]), values(rasters[[5]]) )
			} else if (length(rasters) == 6) {
				vals <- fun( values(rasters[[1]]), values(rasters[[2]]), values(rasters[[3]]), values(rasters[[4]]), values(rasters[[5]]), values(rasters[[6]]) )
			}
			if (filename(outraster) == "") {
				v <- c(v, vals)
			} else {
				outraster <- setValues(outraster, vals, r)
				outraster <- writeRaster(outraster, overwrite=overwrite)
			}	
		}
		if (filename(outraster) == "") { 
			outraster <- setValues(outraster, v) 
		}
	} 
	return(outraster)
}

