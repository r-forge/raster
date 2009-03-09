# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3



setMethod('overlay', signature(x='RasterLayer', y='missing'), 
function(x, y, fun=sum, filename="", overwrite=FALSE, filetype='raster', datatype='FLT4S', track=-1){ 
	return(calc(x, fun=fun, filename=filename, overwrite=overwrite, filetype=filetype, datatype=datatype, track=track))
}
)


setMethod('overlay', signature(x='RasterLayer', y='RasterLayer'), 
function(x, y, ..., fun=sum, filename="", overwrite=FALSE, filetype='raster', datatype='FLT4S', track=-1){ 
	rasters <- c(x, y)
	obs <- list(...)
	if (isTRUE(length(obs) > 0)) {
		for (i in 1:length(obs)) {
			if (extends(class(obs[[i]]), "RasterLayer")) {
				rasters <- c(rasters, obs[[i]])
			} else {
				stop(paste("only RasterLayer objects allowed as ... arguments. Problem:", obs[[i]]))
			}
		}
	}
	return(overlay(rasters, fun=fun, filename=filename, overwrite=overwrite, filetype=filetype, datatype=datatype, track=track))
}
)



setMethod('overlay', signature(x='list', y='missing'), 
function(x, y, fun=sum, filename="", overwrite=FALSE, filetype='raster', datatype='FLT4S', track=-1){ 
	if (missing(fun)) { stop("you must supply a function 'fun'. E.g., 'fun=function(x,y){return(x+y)}'") }
	if (missing(filename)) { filename <- "" }
	if (missing(overwrite)) { overwrite <- FALSE }
	if (missing(datatype)) {datatype='FLT4S'}
	if (missing(track)) {track=-1}
	
	compare(x)

	outraster <- setRaster(x[[1]], filename)
	outraster <- setDatatype(outraster, datatype) 

	inram <- TRUE
	for (i in 1:length(x)) {
		if (dataContent(x[[i]]) != 'all') {
			inram <- FALSE
		} 
	}	
	
	vallist <- list()

	if ( inram ) {
		for (i in 1:length(x)) {
			vallist[[i]] <- values(x[[i]])
			clearValues(x[[i]])
		}
		vals <- do.call(fun, vallist)
		
		outraster <- setValues(outraster, vals)
		if (filename(outraster) != "") { 
			outraster <- writeRaster(outraster, overwrite=overwrite, filetype=filetype) 
		}
		
	} else {
		if (filename(outraster) == "") {
			if (!canProcessInMemory(outraster, 4)) {
				filename <- tempfile()
				outraster <- setFilename(outraster, filename )
			} else {
				v  <- vector(length=ncell(outraster))
				endcell <- 0
				inccol <- ncol(outraster) - 1
			}
		}	
		starttime <- proc.time()

		for (r in 1:nrow(outraster)) {
			for (i in 1:length(x)) {
				if (dataSource(x[[i]]) == 'ram') {
					x[i] <- valuesRow(x[[i]], r)
				} else {	
					x[i] <- readRow(x[[i]], r)
				}	
			}	
			
			for (i in 1:length(x)) {
				vallist[[i]] <- values(x[[i]])
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
				outraster <- writeRaster(outraster, filetype=filetype, overwrite=overwrite)
			}	
			
			if (r %in% track) { .showTrack(r, track, starttime) }
			
		}
		if (filename(outraster) == "") { 
			outraster <- setValues(outraster, v) 
		}
	} 
	return(outraster)

	
}
)
