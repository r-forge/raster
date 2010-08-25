# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date:  August 2010
# Version 1
# Licence GPL v3


stackApply <- function(x, indices, fun, filename='', ...) {

	nl <- nlayers(x)
	if (nl == 1) { 
		return( calc(x, fun=fun, filename=filename, ...) )
	}
	
	ind <- vector(length=nl)
	# perhaps we need recycling:
	ind[] <- indices
	
	uin <- unique(ind)
	nlout <- length(uin)
	if (nlout > 1) {
		out <- brick(x)
	} else {
		out <- raster(x)
	}
	
	filename <- trim(filename)
	if (!canProcessInMemory(out, nlout) & filename == '') {
		filename <- rasterTmpFile()
	} 
	
	if (filename == '') {
		v <- matrix(NA, nrow=ncell(out), ncol=nlout)
	} else {
		out <- writeStart(out, filename=filename, ...)
	}
	tr <- blockSize(out)
	pb <- pbCreate(tr$n, type='') #.progress(...))			

	for (i in 1:tr$n) {
		a <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
		
		if (filename != "") {
			v <- matrix(nrow=tr$nrows[i] * out@ncols, ncol=nlout)
		} else {
			start <- 1 + (tr$row[i]-1) * out@ncols
			end <- start + tr$nrows[i] * out@ncols - 1
		}

		for (j in uin) {
			k <- which(ind == j)
			sv <- apply(a[,k,drop=FALSE], 1, fun)
			if (filename == "") {
				v[start:end, j] <- sv
			} else {
				v[,j] <- sv
			}
		}
		if (filename != "") {
			outraster <- writeValues(outraster, v, tr$row[i])
		}
		pbStep(pb) 
	}

	if (filename == "") { 	
		out <- setValues(out, v)		
	} else {
		out <- writeStop(out)
	}

	pbClose(pb)
	return(out)
}
	

