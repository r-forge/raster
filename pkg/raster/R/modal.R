# Author: Robert J. Hijmans 
# Date :  October 2008
# revised: October 2011, May 2015
# Version 1.0
# Licence GPL v3


setGeneric("modal", function(x, ...)
	standardGeneric("modal"))
	
setMethod('modal', signature(x='ANY'), 
function(x, ..., ties='random', na.rm=FALSE, freq=FALSE) {
#partly based on http://wiki.r-project.org/rwiki/doku.php?id=tips:stats-basic:modalvalue
	x <- c(x, ...)

	# NA itself cannot be the modal value
	# perhaps that should be allowed as an option
	z <- x[!is.na(x)]
	
	if (freq) {
	
		if (length(z) == 0) { 
			return(NA) 
		} else if (!na.rm & length(z) < length(x)) { 
			return(NA)	 
		} else if (length(z) == 1) {
			return(1)
		} else {
			return(max( table(z) ))
		}
	}  # else ....
	
	
	if (!ties %in% c('lowest', 'highest', 'first', 'random', 'NA')) {
		stop("the value of 'ties' should be 'lowest', 'highest', 'first', or 'random'")
	}
	
	if (length(z) == 0) { 
		return(NA) 
	} else if (!na.rm & length(z) < length(x)) { 
		return(NA)	 
	} else if (length(z) == 1) {
		return(z)
	} else {
		if (is.numeric(z)) {
			if (ties == 'lowest') {
				w <- .getMode(z, ties=0)
			} else if (ties == 'highest') {
				w <- .getMode(z, ties=1)
			} else if (ties == 'first') {
				w <- .getMode(z, ties=2)				
			} else if (ties == 'random') {
				w <- .getMode(z, ties=3)
			}
			
		} else { # old approach, only for ties = 'NA' now.
			freq <- table(z)
			if (is.numeric(z)){
				w <- as.numeric(names(freq[max(freq)==freq]))		
			} else if (is.logical(z)) {
				w <- as.logical(freq[max(freq)==freq])
			} else {
				w <- names(freq[max(freq)==freq])
			} # ties is NA
			if (length(w) > 1) {
				w <- NA
			} 
		}
		return(w)
	}	
}
)

