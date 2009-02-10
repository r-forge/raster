# R code for changing rasters (spatial data)
# Authors: Robert J. Hijmans and Jacob van Etten
# International Rice Research Institute
#contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0,7
# Licence GPL v3


#resample <- function(raster, xmin, xmax, ymin, ymax, ncols, nrows, method="bilinear", filename="", overwrite=FALSE) {
#	stop("sorry, not implemented yet")
#}



crop <- function(raster, bndbox, filename="", overwrite=FALSE) {
# we could also allow the raster to expand but for now let's not and first make a separate expand function
	bb <- bbIntersect(c(raster, bndbox))
	bb <- snapBbox(bb, raster)
	outraster <- setRaster(raster, filename)
	outraster <- setBbox(outraster, bb, keepres=T)
	
	if (dataContent(raster) == 'all')  {
		first_start_cell <- cellFromXY(raster, c(xmin(outraster) + 0.5 * xres(raster), ymax(outraster) - 0.5 * yres(raster) ))	
		last_start_cell <- cellFromXY(raster, c(xmin(outraster) + 0.5 * xres(raster), ymin(outraster) + 0.5 * yres(raster) ))
		start_cells <- seq(first_start_cell, last_start_cell, by = ncol(raster))
		end_cells <- start_cells + ncol(outraster) - 1
		selected_cells <- as.vector(mapply(seq, start_cells, end_cells))
		outraster <- setValues(outraster, values(raster)[selected_cells])
		outraster <- setMinMax(outraster)
		if (filename(outraster) != "" ) { 
			outraster <- try(writeRaster(outraster, overwrite=overwrite)) 
		}		

	} else if ( dataSource(raster) == 'disk') { 

		first_col <- colFromX(raster, xmin(outraster) + 0.5 * xres(outraster))
		first_row <- rowFromY(raster, ymax(outraster) - 0.5 * yres(outraster))
		last_row <- first_row + nrow(outraster) - 1
		rownr <- 1
		v <- vector(length=0)
		for (r in first_row:last_row) {
			raster <- readPartOfRow(raster, r, first_col, ncol(outraster) )
			if (filename(outraster) == '') {
				v <- c(v, values(raster))
			} else {
				outraster <- setValues(outraster, values(raster), rownr)
				outraster <- writeRaster(outraster, overwrite=overwrite)
			}	
			rownr <- rownr + 1
		} 
		if (filename(outraster) == '') { outraster <- setValues(outraster, v) }

	}
	return(outraster)
}


