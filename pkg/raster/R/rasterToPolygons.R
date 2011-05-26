# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : December 2009
# Version 0.9
# Licence GPL v3


rasterToPolygons <- function(x, fun=NULL, digits=12) {

	if (nlayers(x) > 1) {
		if (!is.null(fun)) {
			stop('you cannot supply a "fun" argument when "x" has multiple layers')
		}
	}

	if (! fromDisk(x) & ! inMemory(x)) {
		xyv <- xyFromCell(x, 1:ncell(x))
		xyv <- cbind(xyv, NA)
	} else if ( canProcessInMemory(x) ) {
		xyv <- cbind(xyFromCell(x, 1:ncell(x)), getValues(x))
		x <- clearValues(x)
		nas <- apply(xyv[,3:ncol(xyv), drop=FALSE], 1, function(x)all(is.na(x)))
		xyv <- xyv[!nas, ]
		if (!is.null(fun)) {
			xyv <- subset(xyv, fun(xyv[,3]))
		}
	} else {
		tr <- blockSize(x)
		xyv <- matrix(ncol=3, nrow=0)
		colnames(xyv) <- c('x', 'y', 'v')
		for (i in 1:tr$n) {
			start <- cellFromRowCol(x, tr$row[i], 1)
			end <- start+tr$nrows[i]*ncol(x)-1
			xyvr <- cbind(xyFromCell(x, start:end), getValues(x, row=tr$row[i], nrows=tr$nrows[i]))
			nas <- apply(xyvr[,3:ncol(xyvr), drop=FALSE], 1, function(x)all(is.na(x)))
			xyvr <- xyvr[!nas, ]
			if (!is.null(fun)) {
				xyvr <- subset(xyvr, fun(xyvr[,3]))
			}
			xyv <- rbind(xyv, xyvr)
		}
	}

	xr <- xres(x)/2
	yr <- yres(x)/2

	cr <- matrix(ncol=10, nrow=nrow(xyv))
	cr[,c(1,4:5)] <- xyv[,1] - xr
	cr[,2:3] <- xyv[,1] + xr
	cr[,c(6:7,10)] <- xyv[,2] + yr
	cr[,8:9] <- xyv[,2] - yr
	cr <- round(cr, digits=digits)
	
	sp <- lapply(1:nrow(cr), function(i) Polygons(list(Polygon( matrix( cr[i,], ncol=2 ) )), i))
	sp <- SpatialPolygons(sp, proj4string=projection(x, FALSE))
	if (ncol(xyv)==3) {
		sp <- SpatialPolygonsDataFrame(sp, data.frame(value=xyv[,3]), match.ID=FALSE)
	} else {
		sp <- SpatialPolygonsDataFrame(sp, data.frame(xyv[,3:ncol(xyv)]), match.ID=FALSE)	
	}
	sp
}

