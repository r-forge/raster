# Author: Robert J. Hijmans
# Date :  October 2011
# Version 1.0
# Licence GPL v3


.focalC <- function(x, w=matrix(1, nc=3, nr=3), fun, filename='', ...) {

	out <- raster(x)
	filename <- trim(filename)
	globll <- .isGlobalLonLat(out)
	
	if (canProcessInMemory(out)) {
		if (missing(fun)) {
			v <- .Call('focal_sum', values(x), w, as.integer(dim(out)), as.integer(globll), NAOK = TRUE, PACKAGE='raster')
		} else {
			v <- .Call('focal_fun', values(x), w, as.integer(dim(out)), as.integer(globll), fun, new.env(), NAOK = TRUE, PACKAGE='raster')
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
			v <- .Call('focal_fun', v, w, as.integer(c(tr$nrows[1]+addr, nc)), as.integer(globll), fun, e, NAOK = TRUE, PACKAGE='raster')
		} else {
			v <- .Call('focal_sum', v, w, as.integer(c(tr$nrows[1]+addr, nc)), as.integer(globll), NAOK = TRUE, PACKAGE='raster')
		}
		out <- writeValues(out, v, 1)
		for (i in 2:(tr$n-1)) {
			v <- getValues(x, row=tr$row[i]-addr, nrows=tr$nrows[i]+(2*addr))
			if (dofun) {
				v <- .Call('focal_fun', v, w, as.integer(c(tr$nrows[i]+(2*addr), nc)), as.integer(globll), fun, e, NAOK = TRUE, PACKAGE='raster')
			} else {
				v <- .Call('focal_sum', v, w, as.integer(c(tr$nrows[i]+(2*addr), nc)), as.integer(globll), NAOK = TRUE, PACKAGE='raster')
			}
			out <- writeValues(out, v[-nc1], tr$row[i])
		}
		i <- tr$n
		v <- getValues(x, row=tr$row[i]-addr, nrows=tr$nrows[i]+addr)
		if (dofun) {
			v <- .Call('focal_fun', v, w, as.integer(c(tr$nrows[i]+addr, nc)), as.integer(globll), fun, e, NAOK = TRUE, PACKAGE='raster')
		} else {
			v <- .Call('focal_sum', v, w, as.integer(c(tr$nrows[i]+addr, nc)), as.integer(globll), NAOK = TRUE, PACKAGE='raster')
		}
		out <- writeValues(out, v[-nc1], tr$row[i])
		out <- writeStop(out)			
			
	}
	return(out)
}


