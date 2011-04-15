# Author: Robert J. Hijmans
# Date : April 2011
# Version 1.0
# Licence GPL v3


..moran <- function(x, directions=8) {
	stopifnot(directions %in% c(4,8))
	# not memory safe	
	adj <- adjacency(x, from=1:ncell(x), toCells=1:ncell(x), directions=8)
	z <- x - cellStats(x, mean)
	wZiZj <- na.omit(z[adj[,1]] * z[adj[,2]])
	z2 <- cellStats(z*z, sum)
	NS0 <- (ncell(z)-cellStats(z, 'countNA')) / length(wZiZj)
	mI <- NS0 * sum(wZiZj) / z2
	return(mI)
}



Moran <- function(x, w=3) {
	z <- x - cellStats(x, mean)
	if (is.matrix(w)) {
		if (min(dim(w) %% 2)==0) {
			stop('dimensions of filter must be uneven')
		}
		if (w[ceiling(dim(w)[1]/2), ceiling(dim(w)[2]/2)] != 0) {
			warning('central cell of weights matrix (filter) was set to zero')
			w[ceiling(dim(w)[1]/2), ceiling(dim(w)[2]/2)] <- 0
		}
		wZiZj <- focalFilter(z, filter=w, fun=sum, na.rm=TRUE, pad=TRUE)
		wZiZj <- overlay(wZiZj, z, fun=function(x,y){ x * y })
	} else {
		wZiZj <- focal(z, ngb=w, fun=sum, na.rm=TRUE)
		wZiZj <- overlay(wZiZj, z, fun=function(x,y){ (x-y) * y })
	}
	wZiZj <- cellStats(wZiZj, sum)
	z2 <- cellStats(z*z, sum)
	n <- ncell(z) - cellStats(z, 'countNA')
	# weights
	if (is.matrix(w)) {
		if (sum(! unique(w) %in% 0:1) > 0) {
			zz <- calc(z, fun=function(x) ifelse(is.na(x), NA ,1))
			W <- focalFilter( zz, filter=w, fun=sum, na.rm=TRUE, pad=TRUE ) 
		} else {
			W <- focalFilter( z, filter=w, fun=function(x, ...){  sum(!is.na(x))-1 }, pad=TRUE )
		}
	} else {
		W <- focal( z, ngb=w, fun=function(x, ...){ sum(!is.na(x))-1 } )
	}
	NS0 <- n / cellStats(W, sum)
	mI <- NS0 * wZiZj / z2
	return(mI)
}


MoranLocal <- function(x, w=3) { 
	
	z  <- x - cellStats(x, mean) 
	#weights
	if (is.matrix(w)) {
		if (min(dim(w) %% 2)==0) {
			stop('dimensions of filter must be uneven')
		}

		if ( w[ceiling(dim(w)[1]/2), ceiling(dim(w)[2]/2)] != 0 ) {
			warning('central cell of weights matrix (filter) was set to zero')
			w[ceiling(dim(w)[1]/2), ceiling(dim(w)[2]/2)] <- 0
		}

		if (sum(! unique(w) %in% 0:1) > 0) {
			zz <- calc(z, fun=function(x) ifelse(is.na(x), NA ,1))
			W  <- focalFilter( zz, filter=w, fun=sum, na.rm=TRUE, pad=TRUE)
		} else {
			W  <- focalFilter( z, filter=w, fun=function(x, ...){ sum(!is.na(x))-1 }, na.rm=TRUE , pad=TRUE)	
		}
		lz <- (focalFilter(z, filter=w, fun=sum, na.rm=TRUE, pad=TRUE) ) / W
		
	} else {
		W  <- focal( z, ngb=w, fun=function(x, ...){ sum(!is.na(x))-1 } )
		lz <- (focal(z, ngb=w, fun=sum, na.rm=TRUE) - z) / W	
	}

	n <- ncell(x) - cellStats(x, 'countNA')
	s2 <-  cellStats(x, sd)^2 
	# adjust variance denominator from n-1 to n 
	s2 <- (s2 * (n-1)) / n 

	(z / s2) * lz
} 





Geary <- function(x, w=3) {
	
	if (!is.matrix(w)) {
		w <- .checkngb(w)
		w <- matrix(1, nr=w[1], nc=(w[2]))
		w[ceiling(dim(w)[1]/2), ceiling(dim(w)[2]/2)] <- 0
	} else {
		if (w[ceiling(dim(w)[1]/2), ceiling(dim(w)[2]/2)] != 0) {
			warning('central cell of weights matrix (filter) was set to zero')
			w[ceiling(dim(w)[1]/2), ceiling(dim(w)[2]/2)] <- 0
		}
	}
	if (min(dim(w) %% 2)==0) {
		stop('dimensions of weights matrix (filter) must be uneven')
	}

	i <- trunc(length(x)/2)+1 

	n <- ncell(x) - cellStats(x, 'countNA')
	
	fun <- function(x,...) sum(w*(x-x[i])^2, ...)
	w2 <- w
	w2[] <- 1
	Eij <- cellStats(focalFilter(x, filter=w2, fun=fun, na.rm=TRUE, pad=TRUE), sum)

	if (sum(! unique(w) %in% 0:1) > 0) {
		x <- calc(x, fun=function(x) ifelse(is.na(x), NA ,1))
		W <- focalFilter(x, filter=w, fun=sum, na.rm=TRUE, pad=TRUE ) 
	} else {
		W <- focalFilter(x, filter=w, fun=function(x, ...){  sum(!is.na(x))-1 }, pad=TRUE )
	}
	z <- 2 * cellStats(W, sum) * cellStats((x - cellStats(x, mean))^2, sum)
	
	(n-1)*Eij/z
}



