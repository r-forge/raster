# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : January 2009
# Version 0.9
# Licence GPL v3


.checkFields <- function(putvals) {
	for (i in 1:ncol(putvals)) {
		if (class(putvals[,i]) == 'factor') {
			ptv <- try( as.numeric(as.character(putvals[,i])) )
			if (class(ptv) == 'try-error') {
				putvals[,i] <- as.numeric(putvals[,i])
			} else {
				putvals[,i] <- ptv
			}
		} else if (class(putvals) == 'character') {
			ptv <- try( as.numeric(as.character(putvals[,i])) )
			if (class(ptv) == 'try-error') {
				stop('field: ', colnames(putvals)[i], ' cannot be converted to a number')
			} else {
				putvals <- ptv
			}
		}
	}
	putvals
}


.p2r <- function(p, x, field=0, background=NA, mask=FALSE, update=FALSE, filename="", ...) {

	filename <- trim(filename)
	if (mask) {
		oldx <- x
	}
	x <- raster(x)
	projp <- projection(p)
	if (! .compareCRS(projp, x)) {
	#	warning('crs or raster and polygons do not match')
	}
	if (projp != "NA") {
		projection(x) = projp
	} 
	
	spbb <- bbox(p)
	rsbb <- bbox(x)
	if (spbb[1,1] >= rsbb[1,2] | spbb[1,2] <= rsbb[1,1] | spbb[2,1] >= rsbb[2,2] | spbb[2,2] <= rsbb[2,1]) {
		stop('polygon and raster have no overlapping areas')
	}
	
	
	if (! is.numeric(field) ) {
		putvals <- .checkFields( p@data[, field] )
	} else if (length(field) > 1) { 
		if (length(field) == npol) {
			putvals <- field
		} else {
			putvals <- .checkFields( p@data[, field] )
		}
	} else if (mask) {
		putvals <- rep(1, length=npol)	
	} else if (field < 0) {
		putvals <- rep(1, length=npol)	
	} else if (field == 0) {
		putvals <- as.integer(1:npol)
	} else {
		putvals <- .checkFields(p@data[, field])
	}
	if (is.vector(putvals)) {
		putvals <- matrix(putvals, ncol=1)
	} else {
		putvals <- as.matrix(putvals)
	}

	nl <- NCOL(putvals)
	if (nl > 1) {
		x <- brick(x, nl=nl)
	}
	
	npol <- length(p@polygons)
	polinfo <- matrix(NA, nrow=npol * 2, ncol=6)
	addpol <- matrix(NA, nrow=500, ncol=6)
	pollist <- list()
	cnt <- 0
	
	for (i in 1:npol) {
		nsubpol <- length(p@polygons[[i]]@Polygons)
		for (j in 1:nsubpol) {
			cnt <- cnt + 1
			if (cnt > dim(polinfo)[1]) { 
				polinfo <- rbind(polinfo, addpol)  
			}
			polinfo[cnt, 1] <- cnt
			rg <- range(p@polygons[[i]]@Polygons[[j]]@coords[,2])
			polinfo[cnt, 2] <- rg[1]
			polinfo[cnt, 3] <- rg[2]
			polinfo[cnt, 4] <- -99
			polinfo[cnt, 5] <- p@polygons[[i]]@Polygons[[j]]@hole 
			polinfo[cnt, 6] <- i
			pollist[cnt] <- p@polygons[[i]]@Polygons[[j]]
		}
	}
	rm(p)
	polinfo <- subset(polinfo, polinfo[,1] <= cnt, drop=FALSE)
	cat('Found', npol, 'region(s) and', cnt, 'polygon(s)\n') 
		
	if (!canProcessInMemory(x)) {
		if (filename == "") {
			filename <- rasterTmpFile()
		}
	}
	if (filename == "") {
		v <- matrix(NA, ncol=nlayers(x), nrow=ncell(x))
	} else {
		x <- writeStart(x, filename=filename, ...)
	}

	pb <- pbCreate(nrow(x), type=.progress(...))
	tr <- blockSize(x)
	for (i in 1:tr$n) {
		
		cells <- cellFromRowCol(x, tr$row[i], 1) : cellFromRowCol(x, tr$row[i]+(tr$nrows[i]-1), ncol(x))
		xy <- xyFromCell(x, cells)

		rrv <- rv <- rep(NA, ncol(xy))
		holes1 <- holes <- rep(FALSE, ncol(xy))
		ry <- range(xy[,2])
		subpol <- subset(polinfo, !(polinfo[,2] > ry[2] | polinfo[,3] < ry[1]), drop=FALSE)
		
		if (nrow(subpol) > 0) { 		
			updateHoles <- FALSE
			lastpolnr <- subpol[1,6]
			for (j in 1:length(subpol[,1])) {
				if (j == nrow(subpol)) { 
					updateHoles <- TRUE 
				} else if (subpol[j+1,6] > lastpolnr) { # new polygon
					updateHoles <- TRUE 
					lastpolnr <- subpol[j+1,6]
				}
				
				poly <- pollist[[subpol[j,1]]]@coords
				
				res <- .Call("R_point_in_polygon_sp", xy[,1], xy[,2], poly[,1], poly[,2], PACKAGE = "sp")
				res <- as.logical(res)
				if ( subpol[j, 5] == 1 ) {
					holes[res] <- TRUE
				} else {
					rv[res] <- subpol[j,6]
				}
				
				if (updateHoles) {
					rv[holes] <- NA
					rrv[!is.na(rv)] <- rv[!is.na(rv)]
					holes <- holes1
					updateHoles = FALSE	
				}		
			}
		}
		
		if (!is.na(background)) { 
			rrv[is.na(rrv)] <- background
		}
		
		if (mask) {
			vals <- getValues(oldx, tr$row[i], tr$nrows[i])
			if (nl == 1) {
				vals <- matrix(vals, ncol=1)
			}
			vals[!is.na(rrv), ] <- NA
		} else if (update) {
			vals <- getValues(oldx, tr$row[i], tr$nrows[i])
			if (nl == 1) {
				vals <- matrix(vals, ncol=1)
			}
			vals[!is.na(rrv), ] <- putvals[!is.na(rrv), ]
		} else {
			vals <- putvals[rrv, ]
		}

		if (filename == "") {
			v[cells,] <- vals
		} else {
			x <- writeValues(x, vals, i)
		}
		pbStep(pb, i)
	}
	pbClose(pb)

	if (filename == "") {
		x <- setValues(x, v)
	} else {
		x <- writeStop(x)
	}
	return(x)
}

#e = .p2r(p, r)

 
 