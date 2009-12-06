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
function(x, y, filename='', ...) {
	test <- try ( y <- extent(y), silent=TRUE )
	if (class(test) == "try-error") {
		stop('Cannot get an Extent object from argument y')
	}

# we could also allow the raster to expand but for now let's not and first make a separate expand function
	bb <- intersectExtent(x, y)
	bb <- alignExtent(bb, x)
	outraster <- raster(x)
	outraster <- setExtent(outraster, bb, keepres=TRUE)
	filename <- trim(filename)
	
	if (dataContent(x) != 'all' & dataSource(x) == 'disk')  {
		if (canProcessInMemory(outraster, 2)) {
			x <- readAll(x)
		}
	}
	if (dataContent(x) == 'all')  {
		col1 <- colFromX(x, xmin(outraster)+0.5*xres(outraster))
		col2 <- colFromX(x, xmax(outraster)-0.5*xres(outraster))
		row1 <- rowFromY(x, ymax(outraster)-0.5*yres(outraster))
		row2 <- rowFromY(x, ymin(outraster)+0.5*yres(outraster))
		x <- values(x, format='matrix')[(row1:row2), (col1:col2)]
		outraster <- setValues(outraster, as.vector(t(x)))
		if (filename != "") { 
			outraster <- writeRaster(outraster, filename=filename, datatype=dataType(x), ...)
		}

	} else if ( dataSource(x) == 'disk') { 

		if (!canProcessInMemory(outraster, 2) && filename == '') {
			filename <- rasterTmpFile()
			if (getOption('verbose')) { cat('writing raster to:', filename)	}						
		}
		if (filename == '') {
			v <- matrix(NA, ncol=nrow(out), nrow=ncol(out))
		}
		
		first_col <- colFromX(x, xmin(outraster) + 0.5 * xres(outraster))
		first_row <- rowFromY(x, ymax(outraster) - 0.5 * yres(outraster))
		last_row <- first_row + nrow(outraster) - 1
		rownr <- 1
		v <- vector(length=0)

		pb <- pbCreate(nrow(outraster), type=.progress(...))
		for (r in first_row:last_row) {
			x <- readPartOfRow( x, r, first_col, ncol(outraster) )
			if (filename == "") {
				v <- c(v, values(x))
			} else {
				outraster <- setValues(outraster, values(x), rownr)
				outraster <- writeRaster(outraster, filename=filename, ...)
			}	
			rownr <- rownr + 1

			pbStep(pb, r) 			
		} 
		if (filename == '') { 
			outraster <- setValues(outraster, v) 
		}
		pbClose(pb)
		
	}
	return(outraster)
}
)

