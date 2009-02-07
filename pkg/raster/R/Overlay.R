# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3



setMethod('overlay', signature(x='RasterLayer', y='RasterLayer'), 
function(x, y, ..., fun=sum, filename="", overwrite=FALSE, asInt = FALSE){ 

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
	
	compare(c(x, rasters))

	outraster <- setRaster(x, filename)
	if (asInt) { outraster <- setDatatype(outraster, 'integer') }

	inram <- TRUE
	for (i in 1:length(rasters)) {
		if (dataContent(rasters[[i]]) != 'all') {inram <- FALSE} 
	}	
	
	vallist <- list()

	if ( inram ) {
		for (i in 1:length(rasters)) {
			vallist[[i]] <- values(rasters[[i]])
			clearValues(rasters[[i]])
		}
		vals <- do.call(fun, vallist)
		
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
			
			for (i in 1:length(rasters)) {
				vallist[[i]] <- values(rasters[[i]])
			#	clearValues(rasters[[i]])
			}
			vals <- do.call(fun, vallist)
			
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
)

