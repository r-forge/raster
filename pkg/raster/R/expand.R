# R code for changing rasters (spatial data)
# Authors: Robert J. Hijmans and Jacob van Etten
# International Rice Research Institute
#contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0,7
# Licence GPL v3


expand <- function(raster, boundingbox, filename="", overwrite=FALSE) {
	bbox <- boundingbox(boundingbox)
	res <- resolution(raster)
# snap points to pixel boundaries
	xmn <- round(bbox[1,1] / res[1]) * res[1]
	xmx <- round(bbox[1,2] / res[1]) * res[1]
	ymn <- round(bbox[2,1] / res[2]) * res[2]
	ymx <- round(bbox[2,2] / res[2]) * res[2]
	
# only expanding here, not cutting
	xmn <- min(xmn, xmin(raster))
	xmx <- max(xmx, xmax(raster))
	ymn <- min(ymn, ymin(raster))
	ymx <- max(ymx, ymax(raster))
	
	outraster <- setRaster(raster, filename)
	outraster <- setBbox(outraster, xmn, xmx, ymn, ymx, keepres=T)

	startrow <- rowFromY(outraster, ymax(raster))
	startcol <- colFromX(outraster, xmin(raster))
	
	if (dataContent(raster) == 'all')  {

		d <- vector(length=ncells(outraster))
		d[] <- NA
		for (r in 1:nrow(raster)) {
			vals <- valuesRow(raster, r) 
			startcell <- (r + startrow -2) * ncol(outraster) + startcol
			d[startcell:(startcell+ncol(raster)-1)] <- vals
			outraster <- setValues(outraster, d)
			if (filename(outraster) != "") {writeValues(outraster, overwrite=overwrite)}
		}

	} else if ( dataSource(raster) == 'disk' ) { 

		v <- vector(length=0)
		d <- vector(length=ncol(outraster))
		for (r in 1:nrow(raster)) {
			raster <- readRow(raster, r)
			vals <- values(raster)
			d[] <- NA
			startcell <- (r + startrow -2) * ncol(outraster) + startcol
			d[startcell:(startcell+ncol(raster)-1)] <- vals

			if (filename(outraster) != '') {
				outraster <- setValuesRow(outraster, d, r)
				outraster <- writeValues(outraster, overwrite=overwrite)
			} else {
				v <- c(v, d)
			}
		}
		if (filename(outraster) == '') { outraster <- setValues(outraster, v) }
	} 
	return(outraster)
}

