# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : February 2010
# Version 0.9
# Licence GPL v3



factorValues <- function(x, v, layer=1, att=NULL) {
	rat <- levels(x)[[layer]]
	i <- match(v, rat$VALUE)
	r <- rat[i, -c(1:2), drop=FALSE]
	rownames(r) <- NULL
	if (!is.null(att)) {
		if (is.character(att)) {
			att <- na.omit(match(colnames(r), att))
			if (length(att)	== 0) {
				warning("att does not includes valid names")
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
	if (! all( colnames(newv)[1:2] == c('VALUE', 'COUNT'))) {
		stop('the first two column names of the raster attributes (factors) data.frame should be "VALUE" and "COUNT"')
	}
	if (! nrow(newv) == nrow(old)) {
		stop('the number of rows in the raster attributes (factors) data.frame is not correct')
	}
	if (! all(sort(newv[,1]) == sort(old[,1]))) {
		stop('the values in the "VALUE" column in the raster attributes (factors) data.frame are not correct')
	}
	for (n in 3:ncol(newv)) {
		newv[, n] <- as.factor(newv[, n])
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



ratify <- function(x, filename='', ...) {
	stopifnot(nlayers(x) == 1)
	x <- round(x)
	f <- freq(x, useNA='no')
	f <- data.frame(f)
	colnames(f) <- toupper(colnames(f))
	x@data@isfactor <- TRUE
	x@data@attributes <- list(f)
	if (filename != '') {
		x <- writeRaster(x, filename, ...)
		# only native format stores this...
		x@data@isfactor <- TRUE
		x@data@attributes <- list(f)	
	}
	return(x)
}


deratify <- function(x, att=NULL, layer=1, complete=FALSE, filename='', ...) {
	rats <- is.factor(x)
	if (!rats[[layer]]) {	
		warning('This layer is not a factor')
		return(x[[layer]])
	}
	
	RAT <- levels(x)[[layer]]
	if (ncol(RAT) <= 3) {
		if (complete) {
			x <- x[[layer]]
			x@data@isfactor <- FALSE
			x@data@attributes <- list()
			return(x)
		} else {
			warning('this layer already has a single factor level (use "complete=TRUE" to remove it)')
			return(x[[layer]])
		}
	}
	
	nms <- colnames(RAT)
	if (!is.null(att)) {
		if (is.character(att)) {
			att <- na.omit(match(att, nms))
			if (length(att) == 0) {
				stop("argument 'att' does not include valid names")
			}
		}
		RAT <- RAT[ , c(1, att), drop=FALSE]
	} 
	
	w <- 2:ncol(RAT)
	subs(x, RAT, by=1, which=w, subsWithNA=TRUE, filename=filename, ...)	
}



