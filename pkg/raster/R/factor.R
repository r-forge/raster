# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : February 2010
# Version 0.9
# Licence GPL v3


.getlevs <- function(x, f) {
	f <- round(f)
	x[x < 1] <- NA
	x[x > length(f)] <- NA
	f[x]
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



setMethod('levels<-', signature(x='Raster'), 
	function(x, value) {

		if (inherits(x, 'RasterLayer')) {
			if (!is.factor(x)) {
				x <- as.factor(x)
			}
			if (is.list(value)) {
				value <- value[[1]]
			}
			stopifnot (is.factor(value) | is.vector(value))
			value <- as.factor(value)
			stopifnot(length(value) == length(levels(x)[[1]]))
			x@data@attributes <- list(value)
			x@data@isfactor <- TRUE 
			x@data@hasRAT <- FALSE
			return(x)
		} 
		
		i <- sapply(value, is.null)
		stopifnot (length(value) == nlayers(x))

		if (inherits(x, 'RasterStack')) {
			if (! all(i)) {
				for (j in which(!i)) {
					if (!(is.factor(value[[j]]) | is.vector(value[[j]]))) {
						stop('the list elements should hold a factor or a vector')
					} else {
						if (!is.factor(x@layers[[j]])) {
							x@layers[[j]] <- as.factor(x)
						}
						stopifnot(length(value[[j]]) == length(levels(x@layers[[j]][[1]])))
						stopifnot (is.factor(value[[j]]) | is.vector(value[[j]]))
						x@layers[[j]]@data@attributes <- list(factor(value[[j]]))
						x@layers[[j]]@data@isfactor <- TRUE 
						x@layers[[j]]@data@hasRAT <- FALSE
					}
				}
			}
			return(x)		
		}
		
		# else RasterBrick
		if (! all(i)) {
			for (j in which(!i)) {
				if (!(is.factor(value[[j]]) | is.vector(value[[j]]))) {
					stop('the list elements should hold a factor or a vector')
				} else {
					value[[j]] <- factor(value[[j]])
				}
			}
		}
		x@data@isfactor <- i
		x@data@attributes  <- value
		return(x)
	}
)



if (!isGeneric("as.factor")) {
	setGeneric("as.factor", function(x)
		standardGeneric("as.factor"))
}


setMethod('as.factor', signature(x='RasterLayer'), 
	function(x) {
		x <- calc(x, function(i) {
					i <- round(i) 
					i[i < 1] <- NA 
					i } 
				)
		x@data@isfactor <- TRUE
		x@data@attributes <- list(factor(unique(x)))
		return(x)
	}
)

