# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : June 2008
# Version 0.8
# Licence GPL v3



dropLayer <- function(rstack, indices) {
	indices <- sort(indices, decreasing=TRUE)
	for (i in 1:length(indices)) {
		index <- -1 * indices[i]
		rstack@layers <- rstack@layers[index]
		rstack@data@nlayers <- as.integer(rstack@data@nlayers - 1)
		if (dataContent(rstack) == 'all') {
			rstack@data@values <- rstack@data@values[,index, drop=FALSE]
		}
	}	
	return(rstack)
}

