

adjacentUD <- function(x, cells, ngb, pairs=F, target) {

	# ngb should be a matrix with 
	# one and only one cell with value 0 (the focal cell), 
	# at least one cell with value 1 (the adjacent cells)
	# and optionally cells with NA or other values. These are ignored (not considered adjacent)
	
	stopifnot(is.matrix(ngb))
	stopifnot(length(which(ngb==0)) == 1)
	stopifnot(length(which(ngb==1)) > 0)
	ngb[ngb != 0 & ngb != 1] <- NA
	
	x <- raster(x)
	rs <- res(x)
	
	rn <- raster(ngb)
	center <- which(values(rn)==0)
	rc <- rowFromCell(rn, center)
	cc <- colFromCell(rn, center)
	
	xngb <- yngb <- ngb
	xngb[] <- rep(1:ncol(ngb), each=nrow(ngb)) - cc 
	yngb[] <- rep(nrow(ngb):1, ncol(ngb)) - (nrow(ngb)-rc+1)
	xngb <- xngb * rs[1]
	yngb <- yngb * rs[2]
	xngb[is.na(ngb) | ngb==0] <- NA
	yngb[is.na(ngb) | ngb==0] <- NA
	xngb <- as.vector(xngb)
	yngb <- as.vector(yngb)
	
	xy <- xyFromCell(x, cells)
	X <- apply(xy[,1,drop=FALSE], 1, function(x) x + xngb )
	Y <- apply(xy[,2,drop=FALSE], 1, function(x) x + yngb )

	if (raster:::.isGlobalLonLat(x)) {
		# normalize longitude to -180..180
		X[,1] <- (X[,1] + 180) %% 360 - 180
	}

	d <- cbind(as.vector(X), as.vector(Y))
	
	
	if (pairs) {
	#	cells <- rep(cells, directions)
	#	d <- na.omit(cbind(cells, cellFromXY(x, d)))
	#	colnames(d) <- c('from', 'to')
	#	if (!missing(target)) {
	#		d <- d[d[,2] %in% target, ]
	#	}
	#	d <- d[order(d[,1], d[,2]),]
		stop('pairs not yet implemented')
	} else {
		d <- as.vector(unique(na.omit(cellFromXY(x, d))))
		if (!missing(target)) {
			d <- intersect(d, target)
		}
	}
	d
}

