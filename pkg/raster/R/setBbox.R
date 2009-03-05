# R function for the raster package
# Author: Robert J. Hijmans
# International Rice Research Institute. Philippines
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.8
# Licence GPL v3


setExtent <- function(x, bndbox, keepres=FALSE, snap=FALSE) {

	oldbb <- getBbox(x)
	bb <- getBbox(bndbox)
	newobj <- clearValues(x)
	
	if (snap) {
		bb <- alignBbox(bb, newobj)
	}

	newobj@bbox <- bb
	
	if (keepres) {
		xrs <- xres(x)
		yrs <- yres(x)
		nc <- as.integer(round( (xmax(newobj) - xmin(newobj)) / xrs ))
		if (nc < 1) { stop( "xmin and xmax are less than one cell apart" ) 
		} else { newobj@ncols <- nc }
		nr <- as.integer(round( (ymax(newobj) - ymin(newobj)) / yrs ) )
		if (nr < 1) { stop( "ymin and ymax are less than one cell apart" )
		} else { newobj@nrows <- nr }
		newobj@bbox@xmax <- newobj@bbox@xmin + ncol(newobj) * xrs
		newobj@bbox@ymax <- newobj@bbox@ymin + nrow(newobj) * yrs
		
		if (dataContent(x) == 'all') {
			indices <- cellsFromBbox(x, bb)
			newobj <- setValues(newobj, values(x)[indices])
		}
	} else if (class(x) != "BasicRaster") {
		if (ncol(x)==ncol(newobj) & nrow(x)==nrow(newobj))  {
			if (dataContent(x) == 'all') {
				newobj <- setValues(newobj, values(x))
			}	
		}
	}
	return(newobj)
}
