# Authors: Robert J. Hijmans and Jacob van Etten
# contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric("crop")) {
	setGeneric("crop", function(x, y, ...)
		standardGeneric("crop"))
}	


setMethod('crop', signature(x='RasterLayer', y='ANY'), 
function(x, y, filename='', datatype=dataType(x), ...) {
	filename <- trim(filename)

	test <- try ( y <- extent(y), silent=TRUE )
	if (class(test) == "try-error") {
		stop('Cannot get an Extent object from argument y')
	}

# we could also allow the raster to expand but for now let's not and first make a separate expand function
	bb <- intersectExtent(x, y)
	bb <- alignExtent(bb, x)
	outraster <- raster(x)
	outraster <- setExtent(outraster, bb, keepres=TRUE)
	col1 <- colFromX(x, xmin(outraster)+0.5*xres(outraster))
	col2 <- colFromX(x, xmax(outraster)-0.5*xres(outraster))
	row1 <- rowFromY(x, ymax(outraster)-0.5*yres(outraster))
	row2 <- rowFromY(x, ymin(outraster)+0.5*yres(outraster))
	
	if (dataContent(x) != 'all' & dataSource(x) == 'disk')  {
		if (canProcessInMemory(x, 3)) {
			x <- readAll(x)
		}
	}
	datatype=dataType(x)

	if (dataContent(x) == 'all')  {
		x <- values(x, format='matrix')[(row1:row2), (col1:col2)]
		outraster <- setValues(outraster, as.vector(t(x)))
		if (filename != "") { 
			outraster <- writeRaster(outraster, filename=filename, datatype=datatype, ...)
		}

	} else if ( dataSource(x) == 'disk') { 
		nc <- ncol(outraster)
		nr <- row2 - row1 + 1
		if (canProcessInMemory(outraster, 3)) {
			v <- values(.readRasterLayerValues(x, row1, nrows=nr, startcol=col1, ncols=nc))
			outraster <- setValues(outraster, as.vector(v) )
			if (filename != '') { 
				outraster <- writeRaster(outraster, filename=filename, datatype=datatype, ...) 
			}
			return(outraster)
		}
	
		if (!canProcessInMemory(outraster, 2) && filename == '') {
			filename <- rasterTmpFile()
			if (getOption('verbose')) { cat('writing raster to:', filename)	}						
		}
		if (filename == '') {
			v <- matrix(NA, ncol=nrow(outraster), nrow=ncol(outraster))
		}
		
		rownr <- 1
		pb <- pbCreate(nrow(outraster), type=.progress(...))
		for (r in row1:row2) {
			vv <- getValues(x, r)[col1:col2]
			if (filename == "") {
				v[,r] <- vv
			} else {
				outraster <- setValues(outraster, vv, rownr)
				outraster <- writeRaster(outraster, filename=filename, datatype=datatype, ...)
			}	
			rownr <- rownr + 1
			pbStep(pb, r) 			
		} 
		if (filename == '') { 
			outraster <- setValues(outraster, as.vector(v) )
		}
		pbClose(pb)
		
	}
	return(outraster)
}
)

