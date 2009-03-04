# R code for changing rasters (spatial data)
# Authors: Robert J. Hijmans and Jacob van Etten
# International Rice Research Institute
#contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0,7
# Licence GPL v3


expand <- function(raster, bndbox, filename=NULL, filetype='raster', overwrite=FALSE, track=-1)  {
	if (is.null(filename)) { filename <- "" }
	
	bndbox <- getBbox(bndbox)
	res <- resolution(raster)
# snap points to pixel boundaries
	xmn <- round(xmin(bndbox) / res[1]) * res[1]
	xmx <- round(xmax(bndbox) / res[1]) * res[1]
	ymn <- round(ymin(bndbox) / res[2]) * res[2]
	ymx <- round(ymax(bndbox) / res[2]) * res[2]
	
# only expanding here, not cutting
	xmn <- min(xmn, xmin(raster))
	xmx <- max(xmx, xmax(raster))
	ymn <- min(ymn, ymin(raster))
	ymx <- max(ymx, ymax(raster))
	
	outraster <- setRaster(raster, filename)
	bndbox <- newBbox(xmn, xmx, ymn, ymx)
	outraster <- setBbox(outraster, bndbox, keepres=T)

	startrow <- rowFromY(outraster, ymax(raster))
	startcol <- colFromX(outraster, xmin(raster))
	
	if (dataContent(raster) == 'all')  {

		d <- vector(length=ncell(outraster))
		d[] <- NA
		for (r in 1:nrow(raster)) {
			vals <- valuesRow(raster, r) 
			startcell <- (r + startrow -2) * ncol(outraster) + startcol
			d[startcell:(startcell+ncol(raster)-1)] <- vals
			outraster <- setValues(outraster, d)
			if (filename(outraster) != "") {writeRaster(outraster, filetype=filetype, overwrite=overwrite)}
		}

	} else if ( dataSource(raster) == 'disk' ) { 
		if (!.CanProcessInMemory(outraster, 2) && filename == '') {
			filename <- tempfile()
			outraster <- setFilename(outraster, filename )
			if (options('verbose')[[1]]) { cat('writing raster to:', filename(raster))	}						
		}
		starttime <- proc.time()
	
		v <- vector(length=0)
		d <- vector(length=ncol(outraster))
		for (r in 1:nrow(raster)) {
		
			raster <- readRow(raster, r)
			vals <- values(raster)
			d[] <- NA
			startcell <- (r + startrow -2) * ncol(outraster) + startcol
			d[startcell:(startcell+ncol(raster)-1)] <- vals

			if (filename(outraster) != '') {
				outraster <- setValues(outraster, d, r)
				outraster <- writeRaster(outraster, filetype=filetype, overwrite=overwrite)
			} else {
				v <- c(v, d)
			}

			if (r %in% track) {
				elapsed <- (proc.time() - starttime)[3]
				tpr <- elapsed /r
				ttg <- round(tpr/60 * (nrow(raster) - r), digits=1)
				cat('row', r, '-', ttg, 'minutes to go\n')
			}			

		}
		if (filename(outraster) == '') { 
			outraster <- setValues(outraster, v) 
		}
	} 
	return(outraster)
}

