

clusterRaster <- function(x, fun, args=NULL, filename='', cl=NULL, ...) {
	if (is.null(cl)) {
		cl <- getCluster()
		on.exit( returnCluster() )
	}

	nodes <- length(cl)
	tr <- blockSize(x, minblocks=nodes*2 )
	if (tr$n < nodes) {
		nodes <- tr$n
	}
	
	tr$row2 <- tr$row + tr$nrows - 1
	pb <- pbCreate(tr$n, type=raster:::.progress(...))
	out <- raster(x)
	
	myfun <- function(fun, i) {
		r <- crop(x, extent(out, r1=tr$row[i], r2=tr$row2[i], c1=1, c2=ncol(out)))
		r <- fun(r)
		getValues(r)
	}
	
	
	for (i in 1:nodes) {
		sendCall(cl[[i]], myfun, list(fun, i), tag=i)
	}
 
	
	for (i in 1:tr$n) {
		pbStep(pb, i)
		d <- recvOneData(cl)
		if (! d$value$success ) { stop('cluster error') }

		if (i ==1) {
			nl <- NROW(d$value$value) 
			if (nl > 1) {
				out <- brick(out, nl=nl)
			}
			out <- writeStart(out, filename=filename, ...)
		} 
		
		out <- writeValues(out, d$value$value, tr$row[d$value$tag])
		ni <- nodes + i
		if (ni <= tr$n) {
			sendCall(cl[[i]], myfun, list(fun, ni), tag=ni)
		}
	}
	
	return(out)
}
