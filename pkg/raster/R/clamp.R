# Author: Robert J. Hijmans
# Date : July 2013
# Version 1.0
# Licence GPL v3


if (!isGeneric("clamp")) {
	setGeneric("clamp", function(x, ...)
		standardGeneric("clamp"))
}	


setMethod('clamp', signature(x='Raster'), 
function(x, lower=-Inf, upper=Inf, useValues=FALSE, filename='', ...) {
	if (!hasValues(x)) return(x)
	range <- c(as.numeric(lower[1]), as.numeric(upper[1]))
	nl <- nlayers(x)
	if (nl > 1) {
		out <- brick(x, values=FALSE)
	} else {
		out <- raster(x)
	}
	if (canProcessInMemory(out)) {
		out <- setValues(out, .Call('clamp', values(x), range)) 
	} else {
		tr <- blockSize(out)
		pb <- pbCreate(tr$n, label='clamp', ...)
		out <- writeStart(out, filename=filename, ...)
		
		for (i in 1:tr$n) {
			vals <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
			vals <- .Call('clamp', vals, range, as.integer(useValues), NAOK=TRUE, PACKAGE='raster')
			if (nl > 1) {
				vals <- matrix(vals, ncol=nl)
			}
			out <- writeValues(out, vals, tr$row[i])
			pbStep(pb, i)
		}
		out <- writeStop(out)
		pbClose(pb)
	}
	return(out)
}
)

