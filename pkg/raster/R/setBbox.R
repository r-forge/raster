# R function for the raster package
# Author: Robert J. Hijmans
# International Rice Research Institute. Philippines
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.8
# Licence GPL v3



snapBbox <- function(bb, object) {
	oldbb <- getBbox(object)
	bb@xmin <- max(bb@xmin, oldbb@xmin)
	bb@xmax <- min(bb@xmax, oldbb@xmax)
	bb@ymin <- max(bb@ymin, oldbb@ymin)
	bb@ymax <- min(bb@ymax, oldbb@ymax)
	col <- colFromX(object, bb@xmin)
	mn <- xFromCol(object, col) - 0.5 * xres(object)
	mx <- xFromCol(object, col) + 0.5 * xres(object)
	if (abs(bb@xmin - mn) > abs(bb@xmin - mx)) { bb@xmin <- mx } else { bb@xmin <- mn }
	col <- colFromX(object, bb@xmax)
	mn <- xFromCol(object, col) - 0.5 * xres(object)
	mx <- xFromCol(object, col) + 0.5 * xres(object)
	if (abs(bb@xmax - mn) > abs(bb@xmax - mx)) { bb@xmax <- mx } else { bb@xmax <- mn }
	row <- rowFromY(object, bb@ymin)
	mn <- yFromRow(object, row) - 0.5 * yres(object)
	mx <- yFromRow(object, row) + 0.5 * yres(object)
	if (abs(bb@ymin - mn) > abs(bb@ymin - mx)) { bb@ymin <- mx } else { bb@ymin <- mn }
	row <- rowFromY(object, bb@ymax)
	mn <- yFromRow(object, row) - 0.5 * yres(object)
	mx <- yFromRow(object, row) + 0.5 * yres(object)
	if (abs(bb@ymax - mn) > abs(bb@ymax - mx)) { bb@ymax <- mx } else { bb@ymax <- mn }
	return(bb)
}



setBbox <- function(object, bndbox, keepres=FALSE, snap=FALSE) {
	oldbb <- getBbox(object)
	bb <- getBbox(bndbox)
	newobj <- clearValues(object)
	
	if (snap) {
		bb <- snapBbox(bb, newobj)
	}

	newobj@bbox <- bb
	
	if (keepres) {
		xrs <- xres(object)
		yrs <- yres(object)
		nc <- as.integer(round( (xmax(newobj) - xmin(newobj)) / xrs ))
		if (nc < 1) { stop( "xmin and xmax are less than one cell apart" ) 
		} else { newobj@ncols <- nc }
		nr <- as.integer(round( (ymax(newobj) - ymin(newobj)) / xrs ) )
		if (nr < 1) { stop( "ymin and ymax are less than one cell apart" )
		} else { newobj@nrows <- nr }
		newobj@bbox@xmax <- newobj@bbox@xmin + ncol(newobj) * xrs
		newobj@bbox@ymax <- newobj@bbox@ymin + nrow(newobj) * yrs
		
		if (dataContent(object) == 'all') {
			indices <- cellsFromBbox(object, bb)
			newobj <- setValues(newobj, values(object)[indices])
		}
	} else if (class(object) != "BasicRaster") {
		if (ncol(object)==ncol(newobj) & nrow(object)==nrow(newobj))  {
			if (dataContent(object) == 'all') {
				newobj <- setValues(newobj, values(object))
			}	
		}
	}
	return(newobj)
}
