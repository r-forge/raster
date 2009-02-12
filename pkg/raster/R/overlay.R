# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3


setMethod('overlay', signature(x='RasterLayer', y='RasterLayer'), 
function(x, y, ..., fun=sum, filename="", overwrite=FALSE, filetype='raster', datatype='FLT4S', track=-1){ 

	if (missing(fun)) { stop("you must supply a function 'fun'. E.g., 'fun=function(x,y){return(x+y)}'") }
	if (missing(filename)) { filename <- "" }
	if (missing(overwrite)) { overwrite <- FALSE }
	if (missing(datatype)) {datatype='FLT4S'}
	if (missing(track)) {track=-1}
	
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
	
	compare(c(x, rasters))

	outraster <- setRaster(x, filename)
	outraster <- setDatatype(outraster, datatype) 

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
			outraster <- writeRaster(outraster, overwrite=overwrite, filetype=filetype) 
		}
		
	} else {
		if (filename(outraster) == "") {
			if (!.CanProcessInMemory(outraster, 1)) {
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
			
			if (r %in% track) {
				elapsed <- (proc.time() - starttime)[3]
				tpr <- elapsed /r
				ttg <- round(tpr/60 * (nrow(x) - r), digits=1)
				cat('row', r, '-', ttg, 'minutes to go\n')
			}
			
		}
		if (filename(outraster) == "") { 
			outraster <- setValues(outraster, v) 
		}
	} 
	return(outraster)
}
)

