
.metadata <- function(x) {
	x@meta
}

'.metadata<-' <- function(x, value) {
	stopifnot(is.list(value))
	if ( any(unlist(sapply(value, function(x)sapply(x, is.list)))) ) {
		stop('invalid metadata list (too deeply nested)')
	}
	x@meta <- value
	x
}

