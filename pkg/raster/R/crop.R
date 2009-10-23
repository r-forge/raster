# R code for changing rasters (spatial data)
# Authors: Robert J. Hijmans and Jacob van Etten
#contact: r.hijmans@gmail.com
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
	
	if (dataContent(x) == 'all')  {
		first_start_cell <- cellFromXY(x, c(xmin(outraster) + 0.5 * xres(x), ymax(outraster) - 0.5 * yres(x) ))	
		last_start_cell <- cellFromXY(x, c(xmin(outraster) + 0.5 * xres(x), ymin(outraster) + 0.5 * yres(x) ))
		start_cells <- seq(first_start_cell, last_start_cell, by = ncol(x))
		end_cells <- start_cells + ncol(outraster) - 1
		selected_cells <- as.vector(mapply(seq, start_cells, end_cells))
		outraster <- setValues(outraster, values(x)[selected_cells])
		outraster <- setMinMax(outraster)
		if (filename != "") { 
			outraster <- writeRaster(outraster, filename=filename, datatype=dataType(x), ...)
		}

	} else if ( dataSource(x) == 'disk') { 
		
		if (!canProcessInMemory(outraster, 2) && filename == '') {
			filename <- rasterTmpFile()
			if (getOption('verbose')) { cat('writing raster to:', filename)	}						
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