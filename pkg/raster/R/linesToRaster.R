# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3

.specialRowFromY <- function(object, y) {
	rownr <- 1 + (trunc((ymax(object) - y)/yres(object)))
    rownr[y == ymin(object)] <- nrow(object)
    rownr[y > ymax(object)] <- -1
	rownr[y < ymin(object)] <- nrow(object) + 1
	return(rownr)
}

.specialColFromX <- function(object, x) {
	colnr <- (trunc((x - xmin(object))/xres(object))) + 1
    colnr[x == xmax(object)] <- ncol(object)
    colnr[x < xmin(object)] <- -1 
	colnr[x > xmax(object)] <- ncol(object) + 1
    return(colnr)
}


.getCols <- function(rs, rownr, segment, line1, line2) {
# for a simple line (connecting 2 points) and a single poly
	rows <- .specialRowFromY(rs, segment[,2])
	if ((rows[1] > rownr & rows[2] > rownr) | (rows[1] < rownr & rows[2] < rownr)) { return(NA) }
	cols <- .specialColFromX(rs,segment[,1])
	rowcol <- cbind(rows, cols)[order(cols),]
	if (rowcol[1,2] == rowcol[2,2]) {
		return(rowcol[1,2]:rowcol[2,2])
	} else {
		if (rowcol[1,1] == rownr  ) {
			if (rowcol[2,1] < rownr) {
				xy <- .intersectSegments(line1[1,1], line1[1,2], line1[2,1], line1[2,2], segment[1,1], segment[1,2], segment[2,1], segment[2,2]  )
			} else {
				xy <- .intersectSegments(line2[1,1], line2[1,2], line2[2,1], line2[2,2], segment[1,1], segment[1,2], segment[2,1], segment[2,2]  )
			}
			xy <- t(as.matrix(xy))
			cols <- c(rowcol[1,2], colFromX(rs, xy[,1]))
			col1 <- min(cols)
			col2 <- max(cols)
		} else if (rowcol[2,1] == rownr) {
			if (rowcol[1,1] < rownr) {
				xy <- .intersectSegments(line1[1,1], line1[1,2], line1[2,1], line1[2,2], segment[1,1], segment[1,2], segment[2,1], segment[2,2]  )
			} else {
				xy <- .intersectSegments(line2[1,1], line2[1,2], line2[2,1], line2[2,2], segment[1,1], segment[1,2], segment[2,1], segment[2,2]  )
			}		
			xy <- t(as.matrix(xy))
			cols <- c(rowcol[2,2], colFromX(rs, xy[,1]))
			col1 <- min(cols)
			col2 <- max(cols)
		} else {
			xy1 <- .intersectSegments(line1[1,1], line1[1,2], line1[2,1], line1[2,2], segment[1,1], segment[1,2], segment[2,1], segment[2,2]  )
			xy2 <- .intersectSegments(line2[1,1], line2[1,2], line2[2,1], line2[2,2], segment[1,1], segment[1,2], segment[2,1], segment[2,2]  )
			xy <- rbind(xy1, xy2)
			cols <-colFromX(rs, xy[,1])
			col1 <- min(cols)
			col2 <- max(cols)
		}
		return(col1:col2)
	}
}	


linesToRaster <- function(spLines, raster, field=0, filename="", overwrite=FALSE, updateRaster=FALSE, updateValue="NA") {
	filename <- trim(filename)
	if (updateRaster) {
		oldraster <- raster 
		if (!(updateValue == 'NA' | updateValue == '!NA' | updateValue == 'all' | updateValue == 'zero')) {
			stop('updateValue should be either "all", "NA", "!NA", or "zero"')
		}
	}
	raster <- setRaster(raster, filename)

# check if bbox of raster and spLines overlap
	spbb <- bbox(spLines)
	rsbb <- bbox(raster)
	if (spbb[1,1] > rsbb[1,2] | spbb[2,1] > rsbb[2,2]) {
		stop('lines and raster have no overlapping areas')
	}
	nline <- length(spLines@lines)
	info <- matrix(NA, nrow=nline, ncol=3)
	for (i in 1:nline) {
#		holes <- sapply(rings, function(y) slot(y, "hole"))
#		areas <- sapply(rings, function(x) slot(x, "area"))

		info[i,1] <- length(spLines@lines[[i]]@Lines)
		miny <- NULL
		maxy <- NULL
		for (j in 1:info[i,1]) {
			miny <- min(miny, min(spLines@lines[[i]]@Lines[[j]]@coords[,2]))
			maxy <- max(maxy, max(spLines@lines[[i]]@Lines[[j]]@coords[,2]))
		}
		info[i,2] <- miny
		info[i,3] <- maxy
	}
	lxmin <- min(spbb[1,1], rsbb[1,1]) - xres(raster)
	lxmax <- max(spbb[1,2], rsbb[1,2]) + xres(raster)

	if (class(spLines) == 'SpatialPolygons' | field == 0) {
		putvals <- as.integer(1:nline)
	} else {
		putvals <- as.vector(spLines@data[,field])
		if (class(putvals) == 'character') {
			stop('selected field is charater type')
		}
	}
	raster <- setDatatype(raster, class(putvals[1]))
		
	v <- vector(length=0)
	rxmn <- xmin(raster) + 0.1 * xres(raster)
	rxmx <- xmax(raster) - 0.1 * xres(raster)
	for (r in 1:nrow(raster)) {
		rv <- rep(NA, ncol(raster))
		
		ly <- yFromRow(raster, r)
		line1 <- rbind(c(lxmin, ly + 0.5*yres(raster)), c(lxmax,ly + 0.5*yres(raster)))
		line2 <- rbind(c(lxmin, ly - 0.5*yres(raster)), c(lxmax,ly - 0.5*yres(raster)))
		
		uly <- ly + 0.51 * yres(raster)
		lly <- ly - 0.51 * yres(raster)
		for (i in 1:nline) {
			if (info[i,2] > uly | info[i,3] < lly) {
				# do nothing
			} else {
				for (j in 1:info[i,1]) {
					if ( max ( spLines@lines[[i]]@Lines[[j]]@coords[,2] ) < lly  |  min( spLines@lines[[i]]@Lines[[j]]@coords[,2] ) > uly ) {
						# do nothing
					} else {
						aline <- spLines@lines[[i]]@Lines[[j]]@coords
						for (k in 1:(nrow(aline)-1) ) {
							segment <- aline[k:(k+1),]
							colnrs <- .getCols(raster, r, segment, line1, line2)
							if ( length(colnrs) > 0 ) {				
								rv[colnrs] <- putvals[i]
							}
						}
					}
				}
			}
		}
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
			raster <- writeRaster(raster)
		}
	}
	if (filename == "") {
		raster <- setValues(raster, v)
	}
	return(raster)
}

