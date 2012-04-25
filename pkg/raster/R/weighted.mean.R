

	
if (!isGeneric("weighted.mean")) {
	setGeneric("weighted.mean", function(x, w, ...)
		standardGeneric("weighted.mean"))
}	


setMethod('weighted.mean', signature(x='RasterStackBrick', w='vector'), 
	function(x, w, na.rm=FALSE, filename='', ...) {
		stopifnot(length(w) == nlayers(x))
		calc(x, fun=function(i) weighted.mean(i, w=w, na.rm=na.rm), filename=filename, ...)
	}
)


setMethod('weighted.mean', signature(x='RasterStackBrick', w='RasterStackBrick'), 
	function(x, w, na.rm=FALSE, filename='', ...) {
		nlx <- nlayers(x)
		if (nlayers(w) != nlx) {
			stop('nlayers of x and w should be the same')
		}
		out <- raster(x)
		filename <- trim(filename)
		sumw <- sum(w)
		
		if (canProcessInMemory(x, nlx*2)) {
			w <- getValues(w)
			x <- getValues(x)
			w[is.na(x)] <- NA
			x[is.na(w)] <- NA
			
			sumw <- apply(w, 1, sum, na.rm=na.rm)
			w <- apply(w * x, 1, sum, na.rm=na.rm) / sumw
			w <- setValues(out, w)
			
			if (filename != '') {
				writeRaster(w, filename, ...)
			}
			return(w)
			
		} else {
			
			tr <- blockSize(x, n=nls*2)
			pb <- pbCreate(tr$n, ...)
			out <- writeStart(out, filename=filename, ...)
			for (i in 1:tr$n) {
				ww <- getValues(w, row=tr$row[i], nrows=tr$nrows[i])
				xx <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
				ww[is.na(xx)] <- NA
				xx[is.na(ww)] <- NA
				
				wx <- apply(ww * xx, 1, sum, na.rm=na.rm) / apply(ww, 1, sum, na.rm=na.rm)
				out <- writeValues(out, wx, tr$row[i])
				pbStep(pb, i)
			}
			out <- writeStop(out)
			pbClose(pb)
		}
		return(out)
	}
)

