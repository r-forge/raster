# raster package
# Authors: Robert J. Hijmans and Jacob van Etten, r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric("expand")) {
	setGeneric("expand", function(x, extent, ...)
		standardGeneric("expand"))
}	

setMethod('expand', signature(x='RasterLayer', extent='Extent'), 
function(x, extent, filename='', ...) {
	
	bndbox <- extent(extent)
	res <- res(x)
# snap points to pixel boundaries
	xmn <- round(xmin(bndbox) / res[1]) * res[1]
	xmx <- round(xmax(bndbox) / res[1]) * res[1]
	ymn <- round(ymin(bndbox) / res[2]) * res[2]
	ymx <- round(ymax(bndbox) / res[2]) * res[2]
	
# only expanding here, not cutting
	xmn <- min(xmn, xmin(x))
	xmx <- max(xmx, xmax(x))
	ymn <- min(ymn, ymin(x))
	ymx <- max(ymx, ymax(x))
	
	outraster <- raster(x, filename)
	bndbox <- newExtent(xmn, xmx, ymn, ymx)
	outraster <- setExtent(outraster, bndbox, keepres=TRUE)

	startrow <- rowFromY(outraster, ymax(x))
	startcol <- colFromX(outraster, xmin(x))
	
	if (dataContent(x) == 'all')  {

		d <- vector(length=ncell(outraster))
		d[] <- NA
		for (r in 1:nrow(x)) {
			vals <- getValues(x, r) 
			startcell <- (r + startrow -2) * ncol(outraster) + startcol
			d[startcell:(startcell+ncol(x)-1)] <- vals
			outraster <- setValues(outraster, d)
			if (filename != "") {
				outraster <- writeRaster(outraster, filename=filename, datatype=dataType(x), ...)
			}
		}

	} else if ( dataSource(x) == 'disk' ) { 
		if (!canProcessInMemory(outraster, 4) && filename == '') {
			filename <- rasterTmpFile()
			if (getOption('verbose')) { cat('writing raster to:', filename(x))	}						
		}
				
		pb <- pbCreate(nrow(x), type=.progress(...))

		v <- vector(length=0)
		d <- vector(length=ncol(outraster))
		for (r in 1:nrow(x)) {
		
			x <- readRow(x, r)
			vals <- values(x)
			d[] <- NA
			startcell <- (r + startrow -2) * ncol(outraster) + startcol
			d[startcell:(startcell+ncol(x)-1)] <- vals

			if (filename != '') {
				outraster <- setValues(outraster, d, r)
				outraster <- outraster <- writeRaster(outraster, filename=filename, datatype=dataType(x), ...)
			} else {
				v <- c(v, d)
			}

			pbStep(pb, r)
		}
		pbClose(pb)

		if (filename == "") { 
			outraster <- setValues(outraster, v) 
		}
	} 
	return(outraster)
}
)


