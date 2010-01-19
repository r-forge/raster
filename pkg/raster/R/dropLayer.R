# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2008
# Version 0.9
# Licence GPL v3

dropLayer <- function(rstack, indices) {
	i <- sort(unique(round(indices)))
	i <- i[i > 0]
	i <- i[i < (nlayers(rstack)+1)]
	if (length(i) > 0) {
		rstack@layers <- rstack@layers[-i]
		rstack@layernames <- rstack@layernames[-i]
	}
	return(rstack)
}


