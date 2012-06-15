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


.unweightRAT <- function(rat, fun='mean') {
	if (!is.numeric(y)) {
		y <- as.numeric(as.character(y))
	}
	x <- na.omit(rat) 
	if (fun %in% c('min', 'max')) {
		x <- aggregate(x[, 3], x[,1,drop=FALSE], fun)
		x <- data.frame(ID=x[,1], COUNT=NA, x[,2])
	} else if (fun == 'mean') {
		v <- tapply(x[,2] * x[,3], x[,1], sum)
		w <- tapply(x[,2], x[,1], sum)
		x <- v/w
		x <- cbind(ID=as.numeric(names(x)), COUNT=NA, value=x)
	} else {
		stop('not a valid argument for "fun"')
	}
	colnames(x)[3] <- colnames(rat)[3]
	merge(unique(rat[,1,drop=FALSE]), x, by=1, all.x=TRUE)
}



deratify <- function(x, att=NULL, layer=1, complete=FALSE, drop=TRUE, filename='', ...) {

	x <- x[[layer]]
	rats <- is.factor(x)

	if (!rats) {	
		warning('This layer is not a factor')
		return(x)
	}
	
	RAT <- levels(x)[[1]]
	if (colnames(RAT)[2] == 'WEIGHT') {
		weighted <- TRUE
	}

	if (complete) {
		x@data@isfactor <- FALSE
		x@data@attributes <- list()
		return(x)
	}
	
	if (ncol(RAT) <= 3) {
		if (weighted & ncol(RAT)==3) {
			rat <- .unweightRAT(RAT)
		} else {
			warning('this layer already has a single factor level (use "complete=TRUE" to remove it)')
		}
		return(x)
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
	} else {
		RAT <- RAT[, -2]
	}
	
	cc <- 2:ncol(RAT)
	if (drop) {
		for (i in cc) {
			w <- getOption('warn')
			options('warn'=-1) 
			v <- as.numeric(as.character(RAT[,i]))
			options('warn' = w)
			if (isTRUE(all(RAT[,i] == v))) {
				RAT[,i] <- v
			}
		}
	}
	subs(x, RAT, by=1, which=cc, subsWithNA=TRUE, filename=filename, ...)	
}


