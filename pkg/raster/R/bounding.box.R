
changeBbox <- function(object, xmn=xmin(object), xmx=xmax(object), ymn=ymin(object), ymx = ymax(object), keepres=FALSE) {
	bb <- newBbox(xmn, xmx, ymn, ymx) 
	object <- setBbox(object, bb, keepres=keepres) 
	return(object)
}


newBbox <- function(xmn, xmx, ymn, ymx) {
	bb <- new('BoundingBox')
	bb@xmin <- xmn
	bb@xmax <- xmx
	bb@ymin <- ymn
	bb@ymax <- ymx
	return(bb)
}

getBbox <- function(object) {
	if ( class(object) == 'BoundingBox' ) { 
		bb <- object 
	} else if ( class(object) == 'RasterLayer' | class(object) == 'RasterStack' | class(object) == 'RasterBrick' ) {
		bb <- object@bbox
	} else if (class(object) == "matrix") {
		bb <- new('BoundingBox')
		bb@xmin <- object[1,1]
		bb@xmax <- object[1,2]
		bb@ymin <- object[2,1]
		bb@ymax <- object[2,2]
	} else if (class(object) == "vector") {
		bb <- new('BoundingBox')
		bb@xmin <- object[1]
		bb@xmax <- object[2]
		bb@ymin <- object[3]
		bb@ymax <- object[4]
	} else {
		bndbox <- bbox(object)
		bb <- new('BoundingBox')
		bb@xmin <- bndbox[1,1]
		bb@xmax <- bndbox[1,2]
		bb@ymin <- bndbox[2,1]
		bb@ymax <- bndbox[2,2]
	}
	return(bb)
}


setBbox <- function(object, bndbox, keepres=FALSE) {
	xrs <- xres(object)
	yrs <- yres(object)
	object@bbox <- getBbox(bndbox)
	if (keepres) {
		nc <- as.integer(round( (xmax(object) - xmin(object)) / xrs ))
		if (nc < 1) { stop( "xmin and xmax are less than one cell apart" ) 
		} else { object@ncols <- nc }
		nr <- as.integer(round( (ymax(object) - ymin(object)) / xrs ) )
		if (nr < 1) { stop( "ymin and ymax are less than one cell apart" )
		} else { object@nrows <- nr }
		object@bbox@xmax <- object@bbox@xmin + ncol(object) * xrs
		object@bbox@ymax <- object@bbox@ymin + nrow(object) * yrs
	}
	return(object)
}


