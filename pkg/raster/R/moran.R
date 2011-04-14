
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


Moran <- function(x) {
	z <- x - cellStats(x, mean)
	wZiZj <- (focal(z, fun='sum', na.rm=TRUE) - z) * z
	wZiZj <- cellStats(wZiZj, sum)
	z2 <- cellStats(z*z, sum)
	n <- ncell(z) - cellStats(z, 'countNA')
	# weights
	w <- focal( z, fun=function(x, ...){ max(0, sum(!is.na(x))-1) } )
	NS0 <- n / cellStats(w, sum)
	mI <- NS0 * wZiZj / z2
	return(mI)
}


MoranLocal <- function(x) { 
	n <- ncell(x) - cellStats(x, 'countNA')
	s2 <-  cellStats(x, sd)^2 
	# adjust variance denominator from n-1 to n 
	s2 <- (s2 * (n-1)) / n 
	z  <- x - cellStats(x, mean) 
	#weights
	w  <- focal( x, fun=function(x, ...){ max(0, sum(!is.na(x))-1) } )
	lz <- (focal(z, fun='sum', na.rm=TRUE) - z) / w
	(z / s2) * lz
} 

