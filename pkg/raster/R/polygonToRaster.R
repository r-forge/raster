# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  January2008
# Version 0.8
# Licence GPL v3


.intersectSegments <- function(x1, y1, x2, y2, x3, y3, x4, y4) {
# Translated by RH from LISP code by Paul Reiners
# http://local.wasp.uwa.edu.au/~pbourke/geometry/lineline2d/linesegments.lisp
# Which was tranlated from the algorithm by Paul Bourke given here: http://local.wasp.uwa.edu.au/~pbourke/geometry/lineline2d/
    denom  <-  ((y4 - y3) * (x2 - x1)) - ((x4 - x3) * (y2 - y1))
    ua_num  <- ((x4 - x3) *(y1 - y3)) - ((y4 - y3) * (x1 - x3))
    ub_num  <- ((x2 - x1) *(y1 - y3)) - ((y2 - y1) * (x1 - x3))
# If the denominator and numerator for the equations for ua and ub are 0 then the two lines are coincident.
    if ( denom == 0 & ua_num == 0 & ub_num == 0) {
#		return(c(x1, y1))
		xmin <- max(x1, x3)
		if (xmin==x1) {ymin <- y1} else {ymin <- y3}
		xmax <- min(x2, x4)
		if (xmax==x2) {ymax <- y2} else {ymax <- y4}
# RH: for coincident line (segments) returning two intersections : start and end
		return(rbind(c(xmin, ymin),
					 c(xmax, ymax)))
	}	
# If the denominator for the equations for ua and ub is 0 then the two lines are parallel.
    if (denom == 0) {
		return(c(NA, NA))
	}
 	ua <- ua_num / denom
    ub <- ub_num / denom
	if ((ua >= 0 & ua <= 1) & (ub >= 0 & ub <= 1) ) {
        x <- x1 + ua * (x2 - x1)
        y <- y1 + ua * (y2 - y1) 
		return(c(x, y))
	} else {
		return(c(NA, NA))
	}
}


.intersectLinePolygon <- function(line, poly) {
	resxy <- matrix(NA, ncol=2, nrow=0)
	miny <- min(line[,2])
	maxy <- max(line[,2])
	xyxy <- cbind(poly, rbind(poly[-1,], poly[1,]))
    xyxy <- subset(xyxy, !( (xyxy[,2] > maxy & xyxy[,4] > maxy ) | (xyxy[,2] < miny & xyxy[,4] < miny)) )
	if (length(xyxy) < 1) { 
		return(resxy) 
	}
	for (i in 1:length(xyxy[,1])) {
		xy <- .intersectSegments(xyxy[i,1], xyxy[i,2], xyxy[i,3], xyxy[i,4], line[1,1], line[1,2], line[2,1], line[2,2] )
		if (!is.na(xy[1])) {
			resxy <- rbind(resxy, xy)
		}
	}
	return(resxy)
}



polygonsToRaster <- function(spPolys, raster, field=0, filename="", overwrite=FALSE, updateRaster=FALSE, updateValue="NA", trackRows=c(1,2,3,5,10*1:9,100*1:10)) {
	filename <- trim(filename)
	starttime <- proc.time()

	if (updateRaster) {
		oldraster <- raster 
		if (!(updateValue == 'NA' | updateValue == '!NA' | updateValue == 'all' | updateValue == 'zero')) {
			stop('updateValue should be either "all", "NA", "!NA", or "zero"')
		}
	}
	raster <- setRaster(raster, filename)

# check if bbox of raster and spPolys overlap
	spbb <- bbox(spPolys)
	rsbb <- bbox(raster)
	if (spbb[1,1] > rsbb[1,2] | spbb[2,1] > rsbb[2,2]) {
		stop('polygon and raster have no overlapping areas')
	}
	npol <- length(spPolys@polygons)
	info <- matrix(NA, nrow=npol, ncol=3)
	for (i in 1:npol) {
		info[i,1] <- length(spPolys@polygons[[i]]@Polygons)
		miny <- NULL
		maxy <- NULL
		for (j in 1:info[i,1]) {
			miny <- min(miny, min(spPolys@polygons[[i]]@Polygons[[j]]@coords[,2]))
			maxy <- max(maxy, max(spPolys@polygons[[i]]@Polygons[[j]]@coords[,2]))
		}
		info[i,2] <- miny
		info[i,3] <- maxy
	}
	lxmin <- min(spbb[1,1], rsbb[1,1]) - xres(raster)
	lxmax <- max(spbb[1,2], rsbb[1,2]) + xres(raster)

	if (class(spPolys) == 'SpatialPolygons' | field == 0) {
		putvals <- as.integer(1:npol)
	} else {
		putvals <- as.vector(spPolys@data[,field])
		if (class(putvals) == 'character') {
			stop('selected field is charater type')
		}
	}
	raster <- setDatatype(raster, class(putvals[1]))
		
	adj <- 0.5 * xres(raster)
	v <- vector(length=0)
	rxmn <- xmin(raster) + 0.1 * xres(raster)
	rxmx <- xmax(raster) - 0.1 * xres(raster)
	for (r in 1:nrow(raster)) {
		
		rv <- rep(NA, ncol(raster))
		holes <- rep(FALSE, ncol(raster))
		
		ly <- yFromRow(raster, r)
		myline <- rbind(c(lxmin,ly), c(lxmax,ly))
		
		for (i in 1:npol) {
		
			if (info[i,2] > ly | info[i,3] < ly) {
				# entire polygon above or below row. do nothing
			} else {
				for (j in 1:info[i,1]) {
					if ( max ( spPolys@polygons[[i]]@Polygons[[j]]@coords[,2] ) < ly  |  min( spPolys@polygons[[i]]@Polygons[[j]]@coords[,2] ) > ly ) {
						# polygon part above or below row. do nothing
					} else {
						mypoly <- spPolys@polygons[[i]]@Polygons[[j]]@coords
						intersection <- .intersectLinePolygon(myline, mypoly)

						if (nrow(intersection) > 0) {
							x <- sort(intersection[,1])
							for (k in 1:round(nrow(intersection)/2)) {
								l <- (k * 2) - 1		
								x1 <- x[l]
								x2 <- x[l+1]
								if (is.na(x2)) { 
									txt <- paste('something funny at row:', r, 'polygon:',j)
									print(txt)
									warning(txt)
									x2 <- x1 
								}
								if (x1 > rxmx) { next }
								if (x2 < rxmn) { next }
								# adjust to skip first cell if the center is not covered by this polygon
								x1a <- x1 + adj
								x2a <- x2 - adj
								x1a <- min(rxmx, max(rxmn, x1a))
								x2a <- min(rxmx, max(rxmn, x2a))
								col1 <- colFromX(raster, x1a)
								col2 <- colFromX(raster, x2a)
								if (col1 > col2) { next }

								if ( spPolys@polygons[[i]]@Polygons[[j]]@hole ) {
									holes[col1:col2] <- TRUE
								} else {
									rv[col1:col2] <- putvals[i]
								}
							}	
						}
					}	
				}
			}
		}	
		rv[holes] <- NA
		
		if (updateRaster) {
			oldvals <- values(readRow(oldraster, r))
			if (updateValue == "all") {
				ind <- which(!is.na(rv))
			} else if (updateValue == "zero") {
				ind <- which(oldvals==0 & !is.na(rv))
			} else if (updateValue == "NA") {
				ind <- which(is.na(oldvals))
			} else {
				ind <- which(!is.na(oldvals) & !is.na(rv))
			}
			oldvals[ind] <- rv[ind]
			rv <- oldvals
		}

		if (filename == "") {
			v <- c(v, rv)
		} else {
			raster <- setValues(raster, values=rv, rownr=r)
			raster <- writeRaster(raster, overwrite=overwrite)
		}
		
		if (r %in% trackRows) {
			elapsed <- (proc.time() - starttime)[3]
			tpr <- round((elapsed /r), digits=2)
			print(paste('row', r, '--', tpr, 'seconds/row --', nrow(raster)+1-r, " rows to go"))
		}		

	}
	if (filename == "") {
		raster <- setValues(raster, v)
	}
	
	elapsed <- (proc.time() - starttime)[3]
	tpr <- round((elapsed /r), digits=2)
	print(paste('finished in ', round(elapsed/60, digits=1), 'minutes, at', tpr, 'seconds/row'))
	return(raster)
}


polygonsToRaster2 <- function(spPolys, raster, field=0, filename="", overwrite=FALSE) {
#  This is based on sampling by points. Should be slower except when  polygons very detailed and raster las ow resolution
# but it could be optimized further

# this version does not deal with polygon holes 

# check if bbox of raster and spPolys overlap
	filename <- trim(filename)
	raster <- setRaster(raster, filename)

	spbb <- bbox(spPolys)
	rsbb <- bbox(raster)
	if (spbb[1,1] > rsbb[1,2] | spbb[2,1] > rsbb[2,2]) {
		stop('polygon and raster have no overlapping areas')
	}

	if (class(spPolys) == 'SpatialPolygons' | field == 0) {
		putvals <- as.integer(1:length(spPolys@polygons))
	} else {
		putvals <- as.vector(spPolys@data[,field])
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
			over <- overlay(sppoints, spPolys)
			vals <- putvals[over]
		}
		if (filename == "") {
			v <- c(v, vals)
		} else {
			raster <- setValues(raster, vals, r)
			raster <- writeRaster(raster, overwrite=overwrite)
		}
	}
	if (filename == "") {
		raster <- setValues(raster, v)
	}
	return(raster)
}

