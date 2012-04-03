# Author: Robert Hijmans
# Date : October 2008 - December 2011
# Version 1.0
# Licence GPL v3

# April 2012: Patches by Jim Regetz


if (!isGeneric("disaggregate")) {
	setGeneric("disaggregate", function(x, ...)
		standardGeneric("disaggregate"))
}

setMethod('disaggregate', signature(x='Raster'), 
function(x, fact=NULL, method='', filename='', ...) {

	method <- tolower(method)
	if (!method %in% c('bilinear', '')) {
		stop('unknown "method". Should be "bilinear" or ""')
	}
	
	stopifnot(!is.null(fact))
	fact <- round(fact)
	if (length(fact)==1) {
		if (fact == 1) 	return(x)
		if (fact < 2) { stop('fact should be > 1') }
		xfact <- yfact <- fact
	} else if (length(fact)==2) {
		xfact <- fact[1]
		yfact <- fact[2]
		if (xfact < 1) { stop('fact[1] should be > 0') } 
		if (yfact < 1) { stop('fact[2] should be > 0') }
		if (xfact == 1 & yfact == 1) { return(x) }
	} else {
		stop('length(fact) should be 1 or 2')
	}

	filename <- trim(filename)

	nl <- nlayers(x)
	if (nl > 1) {
		out <- brick(x, values=FALSE)
	} else {
		out <- raster(x)
	}
	ncx <- ncol(x)
	nrx <- nrow(x)

	dim(out) <- c(nrx * yfact, ncx * xfact) 
	layerNames(out) <- layerNames(x)
	
	if (! inherits(x, 'RasterStack')) {
		if (! inMemory(x)  & ! fromDisk(x) ) {
			return(out)
		}
	}
	
	if (method=='bilinear') {
		return(resample(x, out, method='bilinear', filename=filename, ...))
	} 
	
	
	if (canProcessInMemory(out, 3)) { 
	
		cols <- rep(rep(1:ncx, each=xfact), times=nrx*yfact)
		rows <- rep(1:nrx, each=ncx*xfact*yfact)
		cells <- (rows-1) * ncx + cols
		
		x <- getValues(x)
		if (nl > 1) {
			x <- x[cells, ]
		} else {
			x <- x[cells]
		}
		out <- setValues(out, x)

		if (filename != '') {
			out <- writeRaster(out, filename=filename,...)
		}
		
	} else { 

		tr <- blockSize(x)
		pb <- pbCreate(tr$n, ...)
		out <- writeStart(out, filename=filename, datatype=dataType(x), ...)
		cols <- rep(seq.int(ncx), each=xfact)
		rows <- rep(seq.int(tr$nrows[1]), each=yfact)
		cells <- as.vector( outer(cols, ncx*(rows-1), FUN="+") )

		for (i in 1:tr$n) {
			if (i == tr$n) {
				if (tr$nrows[i] != tr$nrows[1]) {
					rows <- rep(seq.int(tr$nrows[i]), each=yfact)
					cells <- as.vector( outer(cols, ncx*(rows-1), FUN="+") )
				}
			}
			v <- getValues(x, tr$row[i], tr$nrows[i])
			if (nl > 1) {
				v <- v[cells, ]
			} else {
				v <- v[cells]			
			}
			r <- (tr$row[i]-1) * yfact + 1
			out <- writeValues(out, v, r)
			pbStep(pb, i)
		}
	
		out <- writeStop(out)
		pbClose(pb)

	}

	return(out)
}
)

