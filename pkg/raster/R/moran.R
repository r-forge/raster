# Author: Robert J. Hijmans
# Date : April 2011
# Version 1.0
# Licence GPL v3


.moran <- function(x, directions=8) {
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

# focw <- ngb[ceiling(dim(ngb)[1]/2), ceiling(dim(ngb)[2]/2)]


Moran <- function(x, ngb=3, filter=FALSE) {
	z <- x - cellStats(x, mean)
	if (filter) {
		if (max(dim(ngb) %% 2)) {
			stop('filter must be square')
		}
		if (ngb[ceiling(dim(ngb)[1]/2), ceiling(dim(ngb)[2]/2)] != 0) {
			warning('central cell of weights matrix (filter) was set to zero')
			ngb[ceiling(dim(ngb)[1]/2), ceiling(dim(ngb)[2]/2)] <- 0
		}
		wZiZj <- focalFilter(z, filter=ngb, fun=sum, na.rm=TRUE)	
		wZiZj <- overlay(wZiZj, z, fun=function(x,y){ x * y })
	} else {
		wZiZj <- focal(z, ngb=ngb, fun=sum, na.rm=TRUE)
		wZiZj <- overlay(wZiZj, z, fun=function(x,y){ (x-y) * y })
	}
	wZiZj <- cellStats(wZiZj, sum)
	z2 <- cellStats(z*z, sum)
	n <- ncell(z) - cellStats(z, 'countNA')
	# weights
	if (filter) {
		if (sum(! unique(ngb) %in% 0:1) > 0) {
			zz <- calc(z, fun=function(x) ifelse(is.na(x), NA ,1))
			w <- focalFilter( zz, filter=ngb, fun=function(x, ...){ sum(x, na.rm=TRUE ) } )
		} else {
			w <- focalFilter( z, filter=ngb, fun=function(x, ...){ length(na.omit(x))-1 } )	
		}
	} else {
		w <- focal( z, ngb=ngb, fun=function(x, ...){ sum(!is.na(x))-1 } )
	}
	NS0 <- n / cellStats(w, sum)
	mI <- NS0 * wZiZj / z2
	return(mI)
}


MoranLocal <- function(x, ngb=3, filter=FALSE) { 
	
	z  <- x - cellStats(x, mean) 
	#weights
	if (filter) {
		if (max(dim(ngb) %% 2)) {
			stop('filter must be square')
		}

		if ( ngb[ceiling(dim(ngb)[1]/2), ceiling(dim(ngb)[2]/2)] != 0 ) {
			warning('central cell of weights matrix (filter) was set to zero')
			ngb[ceiling(dim(ngb)[1]/2), ceiling(dim(ngb)[2]/2)] <- 0
		}

		if (sum(! unique(ngb) %in% 0:1) > 0) {
			zz <- calc(z, fun=function(x) ifelse(is.na(x), NA ,1))
			w  <- focalFilter( zz, filter=ngb, fun=function(x, ...){ sum(x, na.rm=TRUE ) } )
		} else {
			w  <- focalFilter( z, filter=ngb, fun=function(x, ...){ length(na.omit(x))-1 } )	
		}
		lz <- (focalFilter(z, filter=ngb, fun=sum, na.rm=TRUE) ) / w
	} else {
		w  <- focal( z, ngb=ngb, fun=function(x, ...){ sum(!is.na(x))-1 } )
		lz <- (focal(z, ngb=ngb, fun=sum, na.rm=TRUE) - z) / w	
	}

	n <- ncell(x) - cellStats(x, 'countNA')
	s2 <-  cellStats(x, sd)^2 
	# adjust variance denominator from n-1 to n 
	s2 <- (s2 * (n-1)) / n 

	(z / s2) * lz
} 

