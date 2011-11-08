# Author: Robert J. Hijmans
# Date : November 2011
# Version 1.0
# Licence GPL v3

if (!isGeneric("edgex")) {
	setGeneric("edgex", function(x, ...)
		standardGeneric("edgex"))
}	

setMethod('edgex', signature(x='RasterLayer'), 
function(x, filename="", classes=TRUE, type='both', asNA=FALSE, asZero=TRUE, ...) {

	stopifnot(nlayers(x) == 1)
	stopifnot(hasValues(x))

	filename <- trim(filename)
	
	out <- raster(x)
	gll <- as.integer( .isGlobalLonLat(out) )

	type <- tolower(type)
	if (! type %in% c('both', 'inner', 'outer'))
		stop("type must be 'both', 'inner', or 'outer'")
		
	if (type=='inner') {
		type <- as.integer(0)
	} else if (type=='outer') {
		type <- as.integer(1)	
	} else {
		type=as.integer(2)
	}
	
	classes <- as.integer(as.logical(classes))
	
	if (asNA) {
		fval <- as.integer(NA)
	} else {
		fval <- as.integer(0)	
	}
	
	if (asZero) {
		asz <- as.integer(1)
	} else {
		asz <- as.integer(0)	
	}

	
	if (canProcessInMemory(out)) {
		x <- as.matrix(x)
		if (gll) {
			x <- cbind(x[, ncol(x)], x, x[, 1]) 
		} else {
			x <- cbind(x[, 1], x, x[, ncol(x)]) 
		}
		x <- rbind(x[1,], x, x[nrow(x),])
		paddim <- as.integer(dim(x))
		x <- .Call('edge', as.integer(t(x)), paddim, classes, type, fval, NAOK=TRUE, PACKAGE='raster')
		x <- matrix(x, nrow=paddim[1], ncol=paddim[2], byrow=TRUE)
		x <- x[2:(nrow(x)-1), 2:(ncol(x)-1)]
		x <- setValues(out, as.vector(t(x)))
		if (filename  != '') {
			x <- writeRaster(x, filename, ...)
		}
		return(x)

	} else {
	
		out <- writeStart(out, filename, ...)
		tr <- blockSize(out, minblocks=3, minrows=3)
		pb <- pbCreate(tr$n, ...)
		
		nc <- ncol(out)+2
		v <- getValues(x, row=1, nrows=tr$nrows[1]+1)
		v <- matrix(v, ncol=tr$nrows[1]+1)
		if (gll) {
			v <- rbind(v[nrow(v),], v, v[1,])
		} else {
			v <- rbind(v[1,], v, v[nrow(v),])
		}
		v <- as.integer(cbind(v[,1], v))
		
		v <- .Call('edge', v, as.integer(c(tr$nrows[1]+2, nc)),  classes, type, fval, NAOK=TRUE, PACKAGE='raster')
		v <- matrix(v, ncol=nc, byrow=TRUE)
		v <- as.integer(t(v[-1, 2:(ncol(v)-1)]))
		out <- writeValues(out, v, 1)
		pbStep(pb, 1)
		for (i in 2:(tr$n-1)) {
			v <- getValues(x, row=tr$row[i]-1, nrows=tr$nrows[i]+2)
			v <- matrix(v, ncol=tr$nrows[1]+1)
			if (gll) {
				v <- rbind(v[nrow(v),], v, v[1,])
			} else {
				v <- rbind(v[1,], v, v[nrow(v),])
			}
			v <- .Call('edge', as.integer(v), as.integer(c(tr$nrows[i]+2, nc)), classes, type, fval, NAOK=TRUE, PACKAGE='raster')
			v <- matrix(v, ncol=nc, byrow=TRUE)
			v <- as.integer(t(v[2:(nrow(v)-1), 2:(ncol(v)-1)]))
			out <- writeValues(out, v, tr$row[i])
			pbStep(pb, i)
		}
		i <- tr$n
		v <- getValues(x, row=tr$row[i]-1, nrows=tr$nrows[i]+1)
		v <- matrix(v, ncol=tr$nrows[1]+1)
		if (gll) {
			v <- rbind(v[nrow(v),], v, v[1,])
		} else {
			v <- rbind(v[1,], v, v[nrow(v),])
		}
		v <- as.integer(cbind(v, v[,ncol(v)]))
		v <- .Call('edge', v, as.integer(c(tr$nrows[i]+1, nc)), classes, type, fval, NAOK=TRUE, PACKAGE='raster')
		v <- matrix(v, ncol=nc, byrow=TRUE)
		v <- as.integer(t(v[2:(nrow(v)-1), 2:(ncol(v)-1)]))
		out <- writeValues(out, v, tr$row[i])
		pbStep(pb, tr$n)

		out <- writeStop(out)
		pbClose(pb)
	}
	return(out)
}
)

