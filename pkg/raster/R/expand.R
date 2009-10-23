# raster package
# Authors: Robert J. Hijmans,  r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric("expand")) {
	setGeneric("expand", function(x, y, ...)
		standardGeneric("expand"))
}	

setMethod('expand', signature(x='RasterLayer', y='ANY'), 
function(x, y, filename='', ...) {

	test <- try ( y <- extent(y), silent=TRUE )
	if (class(test) == "try-error") {
		stop('Cannot get an Extent object from argument y')
	}

	filename <- trim(filename)
	
	bndbox <- extent(y)
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
	
	outraster <- raster(x)
	bndbox <- newExtent(xmn, xmx, ymn, ymx)
	outraster <- setExtent(outraster, bndbox, keepres=TRUE)

	startrow <- rowFromY(outraster, ymax(x))
	startcol <- colFromX(outraster, xmin(x))
	
	if ((dataContent(x) == 'all') | ( dataSource(x) == 'disk' ))  {
		forcetodisk <- FALSE
		if (!canProcessInMemory(outraster, 2) && filename == '') {
			filename <- rasterTmpFile()
			forcetodisk <- TRUE
			if (getOption('verbose')) { cat('writing raster to:', filename)	}						
		}
	}
	
	if (dataContent(x) == 'all')  {

		if (forcetodisk) {
			v <- vector(length=ncol(outraster))
			v[] <- NA		
			datatype <- datatype(x)
			for (r in 1:nrow(x)) {
				vals <- getValues(x, r) 
				if (todisk) {
					vv <- v
					vv[startcol:(startcol+ncol(x)-1)] <- vals
					outraster <- setValues(outraster, vv, r)	
					outraster <- writeRaster(outraster, filename=filename, datatype=datatype, ...)
				}
			}
		} else {
			d <- vector(length=ncell(outraster))
			d[] <- NA
			cells <- cellsFromExtent(outraster, extent(x))
			d[cells] <- values(x)
			outraster <- setValues(outraster, d)	
			if (filename != '') {
				outraster <- writeRaster(outraster, filename=filename, datatype=dataType(x), ...)
			}
		}
		
	} else if ( dataSource(x) == 'disk' ) { 
				
		pb <- pbCreate(nrow(x), type=.progress(...))

		v <- vector(length=0)
		d <- vector(length=ncol(outraster))
		datatype <- datatype(x)
		for (r in 1:nrow(x)) {
		
			x <- readRow(x, r)
			vals <- values(x)
			d[] <- NA
			startcell <- (r + startrow -2) * ncol(outraster) + startcol
			d[startcell:(startcell+ncol(x)-1)] <- vals

			if (filename != '') {
				outraster <- setValues(outraster, d, r)
				outraster <- outraster <- writeRaster(outraster, filename=filename, datatype=datatype, ...)
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


