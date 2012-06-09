# Author: Robert J. Hijmans
# Date : November 2008
# Version 1.0
# Licence GPL v3


	
.xyValues <- function(object, xy, method='simple', cellnumbers=FALSE, buffer=NULL, fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, ...) { 

	nlyrs <- nlayers(object)
	if (nlyrs > 1) {
		if (missing(layer)) { layer <- 1 } 
		if (missing(nl)) { nl <- nlyrs } 
		layer <- min(max(1, round(layer)), nlyrs)
		nl <- min(max(1, round(nl)), nlyrs-layer+1)
	} else {
		layer <- 1
		nl <- 1
	}
	
	if (dim(xy)[2] != 2) {
		stop('xy should have 2 columns only.\nFound these dimensions: ', paste(dim(xy), collapse=', ') )
	}
		
	if (! is.null(buffer)) {
		if (method != 'simple') { 
			warning('method argument is ignored when a buffer is used') 
		}
		value <- .xyvBuf(object, xy, buffer, fun, na.rm, layer=layer, nl=nl, cellnumbers=cellnumbers) 		
		
	} else if (method == 'bilinear') {
		value <- .bilinearValue(object, xy, layer=layer, n=nl) 
		if (cellnumbers) {
			warning("'cellnumbers' does not apply for bilinear values")
		}

	} else if (method=='simple') {
		cells <- cellFromXY(object, xy)
		value <-  .cellValues(object, cells, layer=layer, nl=nl) 
		if (cellnumbers) {			
			value <- cbind(cells, value)
			if (ncol(value) == 2) {
				colnames(value)[2] <- names(object)[layer]
			} 
		}
			
	} else {
		stop('invalid "method" argument. Should be simple or bilinear.')
	}
	
	if (df) {
		if (is.list(value)) {
			value <- data.frame( do.call(rbind, 
				lapply(1:length(value), function(x) if (!is.null(value[[x]])) cbind(ID=x, value[[x]]))) )
		} else {
			if (!is.matrix(value)) {
				value <- matrix(value)
				names(value) <- names(object)
			}
			value <- data.frame(cbind(ID=1:NROW(value), value))
			
			facts <- is.factor(object)[lyrs]
			if (any(facts)) {
				i <- which(facts)
				levs <- levels(object)
				lyrs <- layer:(layer+nl-1)
				for (j in i) {
					k <- lyrs[j]
					value[, j+1] <- .getlevs(value[, j+1], levs[[k]][[1]])
				}
			}
		}
	}
	
	value
}


