
setMethod('setValues', signature(x='BigRasterLayer'), 
function(x, values) {

	if (length(values) == 1) {	
		values <- matrix(rep(values, ncell(x)), ncol=ncol(x))
	}
	if (is.matrix(values)) { 
		if (ncol(values) == x@ncols & nrow(values) == x@nrows) {
			x@bigtrix[]  <- values
		} else if (ncol(values)==1 | nrow(values)==1) {
			x@bigtrix[]  <- matrix(values, ncol=ncol(x), byrow=TRUE)
		} else {
			stop('cannot use a matrix with these dimensions')
		}
	}
	
	if (length(values) == ncell(x)) { 
		x@data@inmemory <- TRUE
		x@data@fromdisk <- FALSE
		x@file@name <- ""
		x@file@driver <- ""
		x@bigtrix[] <- values
		x@data@min <- min(values, na.rm=TRUE)
		x@data@max <- max(values, na.rm=TRUE)
		x@data@haveminmax <- TRUE
		return(x)
		
	} else {
		stop("length(values) is not equal to ncell(x), or to 1") 
	}
 }
)
	

