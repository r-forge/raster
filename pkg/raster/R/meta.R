
.metadata <- function(x) {
	x@meta
}

'.metadata<-' <- function(x, value) {
	stopifnot(is.list(value))
	if ( any(unlist(sapply(value, function(x)sapply(x, is.list)))) ) {
		stop('invalid metadata: list too deeply nested')
	}
	nms <- c(names(value), unlist(sapply(value, names)))
	if (is.null(names) | any(nms == '')) {
		stop('invalid metadata: list elements without names')	
	}
	x@meta <- value
	x
}

