# Author: Robert J. Hijmans
# Date : February 2010 / June 2012
# Version 1.0
# Licence GPL v3



factorValues <- function(x, v, layer=1, att=NULL) {
	stopifnot(is.factor(x)[layer])
	rat <- levels(x)[[layer]]
	if (!is.data.frame(rat)) {
		rat <- rat[[1]]
	}
	if (colnames(rat)[2]=='WEIGHT') {
		i <- which(match(rat$ID, round(v))==1)
		r <- rat[i, ]
	} else {
		i <- match(round(v), rat$ID)
		r <- rat[i, ]
	}

	rownames(r) <- NULL
	if (!is.null(att)) {
		if (is.character(att)) {
			att <- na.omit(match(att, colnames(r)))
			if (length(att)	== 0) {
				warning("att does not includes valid names")
			} else {
				r <- r[, att, drop=FALSE]
			}
		} else {
			r <- r[, att, drop=FALSE]
		}
	}
	r
}


	
if (!isGeneric("is.factor")) {
	setGeneric("is.factor", function(x)
		standardGeneric("is.factor"))
}	

setMethod('is.factor', signature(x='Raster'), 
	function(x) {
		return(x@data@isfactor)
	}
)

setMethod('is.factor', signature(x='RasterStack'), 
	function(x) {
		sapply(x@layers, function(x) x@data@isfactor)
	}
)


if (!isGeneric("levels")) {
	setGeneric("levels", function(x)
		standardGeneric("levels"))
}	

setMethod('levels', signature(x='Raster'), 
	function(x) {
		f <- is.factor(x)
		if (any(f)) {
			if (inherits(x, 'RasterStack')) {
				return( lapply(x@layers, function(i) i@data@attributes)  )
			} else {
				return(x@data@attributes)
			}
		} else {
			return(NULL)
		}
	}
)



.checkLevels <- function(old, newv) {
	if (! is.data.frame(newv)) { 
		stop('new raster attributes (factor values) should be in a data.frame (inside a list)')
	}
	if (! ncol(newv) > 2) {
		stop('the number of columns in the raster attributes (factors) data.frame should be > 2')
	}
	if (! colnames(newv)[1] == c('ID')) {
		stop('the first column name of the raster attributes (factors) data.frame should be "ID"')
	}

	if (colnames(newv)[2] == 'WEIGHT') {
		if (nrow(newv) < nrow(old)) {
			warning('the number of rows in the raster attributes (factors) data.frame is lower than expected (values missing?)')
		}
		if (! all(unique(sort(newv[,1])) == sort(unique(old[,1])))) {
			warning('the values in the "ID" column in the raster attributes (factors) data.frame have changed')
		}
		newv[, 1] <- as.integer(newv[, 1])
		newv[, 2] <- as.numeric(newv[, 2])
	
	} else {
	
		if (! nrow(newv) == nrow(old)) {
			warning('the number of rows in the raster attributes (factors) data.frame is unexpected')
		}
		if (! all(sort(newv[,1]) == sort(old[,1]))) {
			warning('the values in the "ID" column in the raster attributes (factors) data.frame have changed')
		}
		for (n in 1:2) {
			newv[, n] <- as.integer(newv[, n])
		}
	}
	newv
}


setMethod('levels<-', signature(x='Raster'), 
	function(x, value) {
		
		stopifnot(any(is.factor(x)))

		if (inherits(x, 'RasterLayer')) {
			if (!is.data.frame(value)) {
				if (is.list(value)) {
					value <- value[[1]]
				}
			}
			value <- .checkLevels(levels(x)[[1]], value)
			x@data@attributes <- list(value)
			return(x)
		} 
		
		i <- sapply(value, is.null)
		stopifnot (length(value) == nlayers(x))

		if (! all(i)) {
			levs <- levels(x)
			for (j in which(!i)) {
				if (!is.factor(x@layers[[j]])) {
					stop('layer ', j, ' is not a factor')
				}
				value[j] <- .checkLevels(levs[[j]], value[[j]])				
			}
			x@data@attributes <- value
			x@data@isfactor <- i
		}
		return(x)		
	}
)




if (!isGeneric("as.factor")) {
	setGeneric("as.factor", function(x)
		standardGeneric("as.factor"))
}


setMethod('as.factor', signature(x='RasterLayer'), 
	function(x) {
		ratify(x)
	}
)


