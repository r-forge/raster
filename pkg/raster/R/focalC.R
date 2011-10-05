# Author: Robert J. Hijmans
# Date :  October 2011
# Version 1.0
# Licence GPL v3


.focalC <- function(x, w=matrix(1, nc=3, nr=3), fun, filename='', ...) {

	out <- raster(x)
	filename <- trim(filename)

	if (length(w) == 1) {
		w=matrix(1, nc=w, nr=w)
	} else if (length(w) == 2) {
		w=matrix(1, nc=w[1], nr=w[2])
	} 
	if (! is.matrix(w) ) {
		stop('w should be a single number, two numbers, or a matrix')
	} 
	d <- dim(w)
	if (d[1] %% 2 == 0 | d[2] %% 2 == 0) {
		stop('w should have uneven dimensions')
	}
	

	gll <- as.integer(.isGlobalLonLat(out))
	if (canProcessInMemory(out)) {
		if (missing(fun)) {
			v <- .Call('focal_sum', values(x), w, as.integer(dim(out)), gll, NAOK=TRUE, PACKAGE='raster')
		} else {
			v <- .Call('focal_fun', values(x), w, as.integer(dim(out)), gll, fun, new.env(), NAOK=TRUE, PACKAGE='raster')
		}
		out <- setValues(out, v)
		if (filename  != '') {
			out <- writeRaster(out, filename, ...)
		}
		
	} else {

		if (filename == '') {
			filename <- rasterTmpFile()
		}
		out <- writeStart(out, filename)
		tr <- blockSize(out, minblocks=3, minrows=3)
		pb <- pbCreate(tr$n, type=.progress(...))

		addr <- floor(nrow(w) / 2)
		addc <- floor(ncol(w) / 2)
		nc <- ncol(out)
		nc1 <- 1:(nc * addc)
		
		if (missing(fun)) {
			dofun <- FALSE
		} else {
			dofun <- TRUE
		}
		
		e <- new.env()
		v <- getValues(x, row=1, nrows=tr$nrows[1]+addr)
		if (dofun) {
			v <- .Call('focal_fun', v, w, as.integer(c(tr$nrows[1]+addr, nc)), gll, fun, e, NAOK=TRUE, PACKAGE='raster')
		} else {
			v <- .Call('focal_sum', v, w, as.integer(c(tr$nrows[1]+addr, nc)), gll, NAOK=TRUE, PACKAGE='raster')
		}
		out <- writeValues(out, v, 1)
		pbStep(pb)
		for (i in 2:(tr$n-1)) {
			v <- getValues(x, row=tr$row[i]-addr, nrows=tr$nrows[i]+(2*addr))
			if (dofun) {
				v <- .Call('focal_fun', v, w, as.integer(c(tr$nrows[i]+(2*addr), nc)), gll, fun, e, NAOK=TRUE, PACKAGE='raster')
			} else {
				v <- .Call('focal_sum', v, w, as.integer(c(tr$nrows[i]+(2*addr), nc)), gll, NAOK=TRUE, PACKAGE='raster')
			}
			out <- writeValues(out, v[-nc1], tr$row[i])
			pbStep(pb) 
		}
		i <- tr$n
		v <- getValues(x, row=tr$row[i]-addr, nrows=tr$nrows[i]+addr)
		if (dofun) {
			v <- .Call('focal_fun', v, w, as.integer(c(tr$nrows[i]+addr, nc)), gll, fun, e, NAOK=TRUE, PACKAGE='raster')
		} else {
			v <- .Call('focal_sum', v, w, as.integer(c(tr$nrows[i]+addr, nc)), gll, NAOK=TRUE, PACKAGE='raster')
		}
		out <- writeValues(out, v[-nc1], tr$row[i])
		pbStep(pb) 
		
		out <- writeStop(out)			
		pbClose(pb)	
	}
	return(out)
}


