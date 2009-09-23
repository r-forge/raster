# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2008
# Version 0.9
# Licence GPL v3

dropLayer <- function(rstack, indices) {
	indices <- sort(indices, decreasing=TRUE)
	for (i in 1:length(indices)) {
		index <- -1 * indices[i]
		rstack@layers <- rstack@layers[index]
	}	
	return(rstack)
}

