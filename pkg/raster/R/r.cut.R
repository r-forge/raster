# R code for changing rasters (spatial data)
# Authors: Robert J. Hijmans and Jacob van Etten
# International Rice Research Institute
#contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0,7
# Licence GPL v3


#r.resample <- function(raster, xmin, xmax, ymin, ymax, ncols, nrows, method="bilinear", filename="", overwrite=FALSE) {
#	stop("sorry, not implemented yet")
#}


r.cut <- function(raster, boundingbox, filename="", overwrite=FALSE) {
# we could also allow the raster to expand but for now let's not and first make a separate expand function
	bb <- bbox(boundingbox)

	xmn <- max(bb[1,1], xmin(raster))
	xmx <- min(bb[1,2], xmax(raster))
	ymn <- max(bb[2,1], ymin(raster))
	ymx <- min(bb[2,2], ymax(raster))
	
	if (xmn == xmx) {stop("xmin and xmax are less than one cell apart")}
	if (ymn == ymx) {stop("ymin and ymax are less than one cell apart")}
	
	outraster <- set.raster(raster, filename)
	outraster <- set.bbox(outraster, xmn, xmx, ymn, ymx, keepres=T)
	
	if (data.content(raster) == 'all')  {
		first_start_cell <- get.cell.from.xy(raster, c(xmn + 0.5 * xres(raster), ymx - 0.5 * yres(raster) ))	
		last_start_cell <- get.cell.from.xy(raster, c(xmn + 0.5 * xres(raster), ymn + 0.5 * yres(raster) ))
		start_cells <- seq(first_start_cell, last_start_cell, by = ncol(raster))
		end_cells <- start_cells + ncol(outraster) - 1
		selected_cells <- as.vector(mapply(seq, start_cells, end_cells))
		outraster <- set.values(outraster, values(raster)[selected_cells])
		outraster <- set.minmax(outraster)
		if (nchar(filename(outraster)) > 0 ) { 
			outraster <- try(write.raster(outraster)) 
		}		

	} else if ( data.source(raster) == 'disk') { 

		first_col <- get.col.from.x(raster, xmn + 0.5 * xres(outraster))
		first_row <- get.row.from.y(raster, ymx - 0.5 * yres(outraster))
		last_row <- first_row + nrow(outraster) - 1
		rownr <- 1
		v <- vector(length=0)
		for (r in first_row:last_row) {
			raster <- read.part.of.row(raster, r, first_col, ncol(outraster) )
			if (filename(outraster) == '') {
				v <- c(v, values(raster))
			} else {
				outraster <- set.values.row(outraster, values(raster), rownr)
				outraster <- write.row(outraster, overwrite)
			}	
			rownr <- rownr + 1
		} 
		if (filename == '') { outraster <- set.values(outraster, v) }

	}
	return(outraster)
}


