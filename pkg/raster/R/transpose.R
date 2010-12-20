# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : December 2010
# Version 1.0
# Licence GPL v3

	
if (!isGeneric("t")) {
	setGeneric("t", function(x)
		standardGeneric("t"))
}	


setMethod('t', signature(x='RasterLayer'), 
	function(x) {
		r <- raster(x)
		e <- eold <- extent(r)
		e@xmin <- eold@ymin
		e@xmax <- eold@ymax
		e@ymin <- eold@xmin
		e@ymax <- eold@xmax
		extent(r) <- e	
		
		dim(r) <- c(ncol(x), nrow(x))
		if (! hasValues(x)) {
			return(r)
		}
		if (canProcessInMemory(x) | inMemory(x)) {
			return(setValues(r, t(as.matrix(x))))
		} else {
			tr <- blockSize(r)
			pb <- pbCreate(tr$n, type=.progress())			
			r <- writeStart(r, filename=rasterTmpFile(), overwrite=TRUE )

			for (i in 1:tr$n) {
				v <- getValuesBlock(x, row=1, nrows=x@nrows, col=tr$row[i], ncols=tr$nrows[i])
				r <- writeValues(r, t(v), tr$row[i])
				pbStep(pb, i) 	
			}

			r <- writeStop(r)
			pbClose(pb)
			return(r)
		}
	}
)

setMethod('t', signature(x='RasterStackBrick'), 
	function(x) {
		b <- brick(x, values=FALSE)
		e <- eold <- extent(r)
		e@xmin <- eold@ymin
		e@xmax <- eold@ymax
		e@ymin <- eold@xmin
		e@ymax <- eold@xmax
		extent(b) <- e	
		dim(b) <- c(ncol(b), nrow(b))
		if (! hasValues(x)) {
			return(b)
		}
		if (canProcessInMemory(x) | inMemory(x)) {
			x <- as.array(x, transpose=TRUE)
			return( brick(x, xmn=xmin(b), xmx=xmax(b), ymn=ymin(b), ymx=ymax(b), crs=projection(b)) )
		} else {
			stop('not yet implemented for large objects')
		}
	}
)

