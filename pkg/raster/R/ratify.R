# Author: Robert J. Hijmans
# Date : June 2012
# Version 1.0
# Licence GPL v3

ratify <- function(x, filename='', ...) {
	stopifnot(nlayers(x) == 1)
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


deratify <- function(x, att=NULL, layer=1, complete=FALSE, drop=TRUE, filename='', ...) {
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
	if (drop) {
		for (i in w) {
			v <- as.numeric(as.character(RAT[,w]))
			if (all(RAT[,w] == v)) {
				RAT[,i] <- v
			}
		}
	}
	subs(x, RAT, by=1, which=w, subsWithNA=TRUE, filename=filename, ...)	
}



