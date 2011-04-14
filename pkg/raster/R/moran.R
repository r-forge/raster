
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


Moran <- function(x, ngb=3, filter=FALSE) {
	z <- x - cellStats(x, mean)
	if (filter) {
		wZiZj <- focalFilter(z, filter=ngb, fun=sum, na.rm=TRUE)	
	} else {
		wZiZj <- focal(z, ngb=ngb, fun=sum, na.rm=TRUE)
	}
	wZiZj <- overlay(wZiZj, z, fun=function(x,y){ (x-y) * y })
	wZiZj <- cellStats(wZiZj, sum)
	z2 <- cellStats(z*z, sum)
	n <- ncell(z) - cellStats(z, 'countNA')
	# weights
	if (filter) {
		if (sum(! unique(filter) %in% 0:1) > 0) {
			zz <- z / z
			w  <- focalFilter( zz, filter=ngb, fun=function(x, ...){ sum(x, na.rm=TRUE ) } )
		} else {
			w <- focalFilter(z, filter=ngb, fun=function(x, ...){ sum(!is.na(x))-1 } )	
		}
	} else {
		w <- focal( z, ngb=ngb, fun=function(x, ...){ sum(!is.na(x))-1 } )
	}
	NS0 <- n / cellStats(w, sum)
	mI <- NS0 * wZiZj / z2
	return(mI)
}


MoranLocal <- function(x, ngb=3, filter=FALSE) { 
	
	n <- ncell(x) - cellStats(x, 'countNA')
	s2 <-  cellStats(x, sd)^2 
	# adjust variance denominator from n-1 to n 
	s2 <- (s2 * (n-1)) / n 
	z  <- x - cellStats(x, mean) 
	#weights
	if (filter) {
		if (sum(! unique(filter) %in% 0:1) > 0) {
			zz <- z / z
			w  <- focalFilter( zz, filter=ngb, fun=function(x, ...){ sum(x, na.rm=TRUE ) } )
		} else {
			w  <- focalFilter( z, filter=ngb, fun=function(x, ...){ sum(!is.na(x))-1 } )
		}
		lz <- (focalFilter(z, filter=ngb, fun=sum, na.rm=TRUE) - z) / w
	} else {
		w  <- focal( z, ngb=ngb, fun=function(x, ...){ sum(!is.na(x))-1 } )
		lz <- (focal(z, ngb=ngb, fun=sum, na.rm=TRUE) - z) / w	
	}
	(z / s2) * lz
} 

