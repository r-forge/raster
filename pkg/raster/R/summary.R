# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
# Date :  January 2009
# Version 0.9
# Licence GPL v3



setMethod("Summary", signature(x='Raster'),
	function(x, ..., na.rm=FALSE){
		rasters <- .makeRasterList(x, ...)
		add <- .addArgs(...)
		
# from callGeneric BEGIN
		frame <- sys.parent()
		envir <- parent.frame()
		call <- sys.call(frame)
		localArgs <- FALSE
		if (exists(".Generic", envir = envir, inherits = FALSE)) {
			fname <- get(".Generic", envir = envir)
		} else {
			localArgs <- identical(as.character(call[[1L]]), ".local")
			if (localArgs) { call <- sys.call(sys.parent(2)) }
			fname <- as.character(call[[1L]])
		}
		fdef <- get(fname, envir = envir)
# from callGeneric END		
		
		if (length(rasters)==1 & length(add)==0) {
			if (fname == 'min' | fname=='max' | fname=='sum' | fname=='prod') {
				return(x)
			} 
			if (fname == 'range') {
				stop('a single layer does not have a range')
			}
		}
		
		rm(x)
		
		if (fname == 'any' | fname == 'all') {
			fun <- function(...){ fdef(as.logical(...), na.rm=na.rm) }
		} else {
			fun <- function(...){ fdef(..., na.rm=na.rm) }
		}
		
		.summaryRasters(rasters, add, fun, funname=fname, ...) 
	}
)

.summaryRasters <- function(rasters, add, fun, funname='', ...) {
	
	outRaster <- raster(rasters[[1]])

	if (!canProcessInMemory(outRaster, length(rasters)+1)) {
		filename <- rasterTmpFile()
	} else {
		filename <- ""
		v <- vector(length=0)
	}
	
	
	m <- matrix(NA, nrow=ncol(outRaster), ncol=length(rasters))
	if (length(add) > 0) {
		add <- matrix(rep(add, each=nrow(m)), nrow=nrow(m))
		m <- cbind(m, add)
	}
	
	
	pb <- pbCreate(nrow(outRaster), type=.progress(...))
	for (r in 1:nrow(outRaster)) {
		for (i in 1:length(rasters)) {
			m[,i] <- getValues(rasters[[i]], r)
		}
		
		vv <- apply(m, 1, fun)
		
		if (class(vv) == 'matrix') { # range
			vv <- vv[2,] - vv[1,]
		}
		
		if (filename == "") {
			v <- c(v, vv)
		} else {
			outRaster <- setValues(outRaster, vv, r)
			outRaster <- writeRaster(outRaster, filename=filename, ...)
		}
		pbStep(pb, r) 
	} 
	pbClose(pb)			
	if (filename == "") {
		outRaster <- setValues(outRaster, v)
	}
	return(outRaster)
}


.addArgs <- function(...) {
	lst <- list(...)
	add <- list()
	if (length(lst) > 0 ) {
		cnt <- 0
		for (i in 1:length(lst)) {
		# is.atomic ?
			if (class(lst[[i]]) %in% c('logical', 'integer', 'numeric')) {
				cnt <- cnt + 1
				if (length(lst[[i]]) > 1) {
					stop('only single numbers can be added as additional arguments')
				} else {
					add[cnt] <- lst[[i]]
				}
			}
		}
	}
	add <- unlist(add)
	return(add)
}

