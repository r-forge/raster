# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,1
# Licence GPL v3

setMethod('overlay', signature(x='RasterLayer', y='RasterLayer'), 
	function(x, y, ...){ 
		return(Overlay(x, y, ...) )
	}
)


Overlay <- function(x, y, ..., fun, filename="", overwrite=FALSE){ 

	if (missing(fun)) { stop("you must supply a function 'fun'. E.g., 'fun=function(x,y){return(x+y)}'") }
	if (missing(filename)) { filename <- "" }
	if (missing(overwrite)) { overwrite <- FALSE }
	
	rasters <- c(x, y)
	obs <- list(...)
	if (isTRUE(length(obs) > 0)) {
		for (i in 1:length(obs)) {
			if (extends(class(obs[[i]]), "RasterLayer")) {
				rasters <- c(rasters, obs[[i]])
			} else {
				stop("only RasterLayer objects allowed as ... arguments.")
			}
		}
	}
	if (length(rasters) > 6) {stop("sorry, this function cannot take more than 6 RasterLayers at a time")}
	
	f <- formals(fun)
	if (length(f) != length(rasters)) {
		stop(paste("Function/data mismatch. You provided a function with", length(f), "arguments. While passing", length(rasters), "RasterLayer objects."))
	}

	
	for (i in 2:length(rasters)) {
		if (!compare(c(x, rasters[i]))) { 
			stop('Extent and/or resolution of rasters do not match') 
		}	
	}
	outraster <- setRaster(x)
	outraster <- setFilename(outraster, filename)

	inram <- TRUE
	ondisk <- TRUE
	for (i in 1:length(rasters)) {
		if (dataContent(rasters[[i]]) != 'all') {inram <- FALSE} 
		if (dataSource(rasters[[i]]) != 'disk') {ondisk <- FALSE} 		
	}	
	
	
	if ( inram ) {
	# there has to be a smarter way then this!
	# perhaps via    as.function(alist( )) ??
	
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
		if (filename(outraster) != "") { 
			writeRaster(outraster, overwrite=overwrite) 
		}
		
	} else {
		if (filename(outraster) == "") {
#			v <- vector(length=0)
			v  <- vector(length=ncell(outraster))
			endcell <- 0
			inccol <- ncol(outraster) - 1
		}	
		
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
#				v <- c(v, vals)
				startcell <- endcell + 1
				endcell <- startcell + inccol
				v[startcell:endcell] <- vals
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

