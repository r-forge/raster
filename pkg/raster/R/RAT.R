

.hasRAT <- function(x) {
	if (.hasSlot(x@data, 'hasRAT')) {
		x@data@hasRAT
	} else {
		FALSE
	}
}


rats <- function(x) {
	stopifnot(.hasRAT(x))
	x@data@attributes[[1]]
}

'rats<-' <- function(x, value) {
	
	stopifnot(is.data.frame(value))
	stopifnot(ncol(value) > 2)
	stopifnot(colnames(value)[1:2] == c('VALUE', 'COUNT'))
	
	x@data@hasRAT <- TRUE
	x@data@isFactor <- TRUE
	x@data@attributes <- list(value)
}


ratToLayer <- function(x, att=NULL, filename='', ...) {
	RAT <- rats(x)
	if (is.null(att)) {
		att <- 3:ncol(RAT)
	}
	ratvs <- RAT[ , att, drop=FALSE]
	if (is.null(ratvs)) {
		stop("'att' is not a valid column in the Raster Attribute Table")
	}
	ratvs <- data.frame(VALUE=RAT$VALUE, ratvs)
	w <- unique(2:ncol(ratvs))
	subs(x, ratvs, by=1, which=w, subsWithNA=TRUE, filename=filename, ...)	
}
#b = ratToLayer(x, 'WHRSIZE')


ratify <- function(x, filename='', ...) {
stop('not yet implemented')
}


