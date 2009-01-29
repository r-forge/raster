# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,1
# Licence GPL v3


.intersectSegments <- function(x1, y1, x2, y2, x3, y3, x4, y4) {
#From LISP code by Paul Reiners
# http://local.wasp.uwa.edu.au/~pbourke/geometry/lineline2d/linesegments.lisp
# From algorithm by Paul Bourke given here: http://local.wasp.uwa.edu.au/~pbourke/geometry/lineline2d/
    denom  <-  ((y4 - y3) * (x2 - x1)) - ((x4 - x3) * (y2 - y1))
    ua_num  <- ((x4 - x3) *(y1 - y3)) - ((y4 - y3) * (x1 - x3))
    ub_num  <- ((x2 - x1) *(y1 - y3)) - ((y2 - y1) * (x1 - x3))
# If the denominator and numerator for the equations for ua and ub are 0 then the two lines are coincident.
    if ( denom == 0 & ua_num == 0 & ub_num == 0) {
#		print("A")
#		return(c(x1, y1))
		xmin <- max(x1, x3)
		if (xmin==x1) {ymin <- y1} else {ymin <- y3}
		xmax <- min(x2, x4)
		if (xmax==x2) {ymax <- y2} else {ymax <- y4}
		return(rbind(c(xmin, ymin), c(xmax, ymax)))
	}	
# If the denominator for the equations for ua and ub is 0 then the two lines are parallel.
    if (denom == 0) {
#		print("B")
		return(NA)
	}
 	ua <- ua_num / denom
    ub <- ub_num / denom
	if ((ua >= 0 & ua <= 1) & (ub >= 0 & ub <= 1) ) {
        x <- x1 + ua * (x2 - x1)
        y <- y1 + ua * (y2 - y1) 
#		print("C")
		return(c(x, y))
	} else {
#		print("D")
		return(NA)
	}
}


.overlayLinePolygon <- function(line, poly) {
# for a simple line (connecting 2 points)
	resxy <- matrix(NA, ncol=2, nrow=0)
	if (min(poly[,2]) > max(line[,2]) | max(poly[,2]) < min(line[,2])) {
		return(resxy)
	} else if (min(poly[,1]) > max(line[,1]) | max(poly[,1]) < min(line[,1])) {
		return(resxy)
	} else {
		for (i in 2:length(poly[,1])) {
			#compute intersection
			xy <- .intersectSegments(poly[i,1], poly[i,2], poly[i-1,1], poly[i-1,2], line[1,1], line[1,2], line[2,1], line[2,2] )
			if (length(xy) > 1) {
				resxy <- rbind(resxy, xy)
			}
		}
		xy <- .intersectSegments(poly[1,1], poly[1,2], poly[length(poly[,1]),1], poly[length(poly[,1]),2], line[1,1], line[1,2], line[2,1], line[2,2] )
		if (length(xy) > 1) {
			resxy <- rbind(resxy, xy)
		}
		return(resxy)
	}
}



polygonsToRaster <- function(sppoly, raster, field=0, filename="", overwrite=FALSE) {
# check if bbox of raster and sppoly overlap
	filename <- trim(filename)
	raster <- setRaster(raster, filename)

	spbb <- bbox(sppoly)
	rsbb <- bbox(raster)
	if (spbb[1,1] > rsbb[1,2] | spbb[2,1] > rsbb[2,2]) {
		stop('polygon and raster have no overlapping areas')
	}
	npol <- length(sppoly@polygons)
	info <- matrix(NA, nrow=npol, ncol=3)
	for (i in 1:npol) {
		info[i,1] <- length(sppoly@polygons[[i]]@Polygons)
		miny <- NULL
		maxy <- NULL
		for (j in 1:info[i,1]) {
			miny <- min(miny, min(sppoly@polygons[[i]]@Polygons[[j]]@coords[,2]))
			maxy <- max(maxy, max(sppoly@polygons[[i]]@Polygons[[j]]@coords[,2]))
		}
		info[i,2] <- miny
		info[i,3] <- maxy
	}
	lxmin <- min(spbb[1,1], rsbb[1,1]) - xres(raster)
	lxmax <- max(spbb[1,2], rsbb[1,2]) + xres(raster)

	if (class(sppoly) == 'SpatialPolygons' | field == 0) {
		putvals <- as.integer(1:npol)
	} else {
		putvals <- as.vector(sppoly@data[,field])
		if (class(putvals) == 'character') {
			stop('selected field is charater type')
		}
	}
	raster <- setDatatype(raster, class(putvals[1]))
		
	adj <- 0.49 * xres(raster)
	v <- vector(length=0)
	rxmn <- xmin(raster) + 0.1 * xres(raster)
	rxmx <- xmax(raster) - 0.1 * xres(raster)
	for (r in 1:nrow(raster)) {
		rv <- rep(NA, ncol(raster))
		ly <- yFromRow(raster, r)
		myline <- rbind(c(lxmin,ly), c(lxmax,ly))
		
		uly <- ly + 0.01 * yres(raster)
		lly <- ly - 0.01 * yres(raster)
		for (i in 1:npol) {
			if (info[i,2] > uly | info[i,3] < lly) {
				# do nothing
			} else {
				for (j in 1:info[i,1]) {
					if ( max ( sppoly@polygons[[i]]@Polygons[[j]]@coords[,2] ) < ly  |  min( sppoly@polygons[[i]]@Polygons[[j]]@coords[,2] ) > ly ) {
						# do nothing
					} else {
						mypoly <- sppoly@polygons[[i]]@Polygons[[j]]@coords
						intersection <- .overlayLinePolygon(myline, mypoly)
						if (nrow(intersection) > 0) {
							x <- sort(intersection[,1])
							for (k in 1:round(nrow(intersection)/2)) {
								l <- (k * 2) - 1		
								x1 <- x[l]
								x2 <- x[l+1]
								if (x1 > rxmx) { next }
								if (x2 < rxmn) { next }
								# adjust to skip first cell if the center is not covered by this polygon
								x1a <- x1 + adj
								x2a <- x2 - adj
								x1a <- max(rxmn, x1a)
								x2a <- min(rxmx, x2a)
								col1 <- colFromX(raster, x1a)
								col2 <- colFromX(raster, x2a)
								if (is.na(col1) | is.na(col2) | col1 > col2) {	next }
								rv[col1:col2] <- putvals[i]
							}	
						}
					}	
				}
			}
		}	
		if (filename == "") {
			v <- c(v, rv)
		} else {
			raster <- setValues(raster, values=rv, rownr=r)
			raster <- writeRaster(raster)
		}
	}
	if (filename == "") {
		raster <- setValues(raster, v)
	}
	return(raster)
}


polygonsToRaster2 <- function(sppoly, raster, field=0, filename="", overwrite=FALSE) {
#  This is based on sampling by points. Should be slower except when  polygons very detailed and raster las ow resolution
# but it could be optimized further

# check if bbox of raster and sppoly overlap
	filename <- trim(filename)
	raster <- setRaster(raster, filename)

	spbb <- bbox(sppoly)
	rsbb <- bbox(raster)
	if (spbb[1,1] > rsbb[1,2] | spbb[2,1] > rsbb[2,2]) {
		stop('polygon and raster have no overlapping areas')
	}

	if (class(sppoly) == 'SpatialPolygons' | field == 0) {
		putvals <- as.integer(1:length(sppoly@polygons))
	} else {
		putvals <- as.vector(sppoly@data[,field])
		if (class(putvals) == 'character') {
			stop('selected field is charater type')
		}
	}
	raster <- setDatatype(raster, class(putvals[1]))
		
	
	v <- vector(length=0)
	rowcol <- cbind(0, 1:ncol(raster))

	firstrow <- rowFromY(raster, spbb[2,2])
	lastrow <- rowFromY(raster, spbb[2,1])
	
	for (r in 1:nrow(raster)) {
		if (r < firstrow | r > lastrow) {
			vals <- rep(NA, times=ncol(raster))
		} else {
			rowcol[,1] <- r
			sppoints <- xyFromCell(raster, cellFromRowCol(raster, rowcol[,1], rowcol[,2]), TRUE)
			over <- overlay(sppoints, sppoly)
			vals <- putvals[over]
		}
		if (filename == "") {
			v <- c(v, vals)
		} else {
			raster <- setValues(raster, vals, r)
			raster <- writeRaster(raster)
		}
	}
	if (filename == "") {
		raster <- setValues(raster, v)
	}
	return(raster)
}

