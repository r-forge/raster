# R function for the raster package
# Author: Robert J. Hijmans
# International Rice Research Institute. Philippines
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.8
# Licence GPL v3


setBbox <- function(object, bndbox, keepres=FALSE, snap=FALSE) {
	oldbb <- getBbox(object)
	bb <- getBbox(bndbox)
	newobj <- clearValues(object)
	
	if (snap) {
		bb <- alignBbox(bb, newobj)
	}

	newobj@bbox <- bb
	
	if (keepres) {
		xrs <- xres(object)
		yrs <- yres(object)
		nc <- as.integer(round( (xmax(newobj) - xmin(newobj)) / xrs ))
		if (nc < 1) { stop( "xmin and xmax are less than one cell apart" ) 
		} else { newobj@ncols <- nc }
		nr <- as.integer(round( (ymax(newobj) - ymin(newobj)) / yrs ) )
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
