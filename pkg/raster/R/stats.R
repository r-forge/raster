# R miscellaneous stat functions
# Authors: Robert J. Hijmans 
# International Rice Research Institute
# contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0,8
# Licence GPL v3


Mode <- function(x, ties='random', na.rm = TRUE) {
#partly based on http://wiki.r-project.org/rwiki/doku.php?id=tips:stats-basic:modalvalue
# ties are broken at random
# earlier approach contained this
#		freq <- tapply(rep(0, length(x)), x, length)
#		w <- as.vector(which(freq == max(freq)) )
#		result <- as.numeric(dimnames(freq)[[1]][w])
#		return(result)
	z <- x[!is.na(x)]
	if (length(z) == 0) { return(NA) 
	} else if (na.rm == FALSE & length(z) < length(x)) { return(NA)	 
	} else if (length(z) == 1) { return(z)
	} else {
		freq <- table(z)
		w <- as.numeric(names(freq[max(freq)==freq]))
		if (length(w) > 1) {
			if (ties == 'lowest') {
				w <- min(w)
			} else if (ties == 'highest') {
				w <- max(w)
			} else if (ties == 'NA') {
				w <- NA
			} else { # random
				r <- runif(length(w))
				w <- w[which.max(r)]
			}	
		} 
		return(w)
	}	
}


CV <- function(x, na.rm = TRUE, singlevalueaszero=TRUE) {
#  R function to compute the coefficient of variation (expressed as a percentage)
# if there is only a single value, sd = NA. However, one could argue that cv =0. In this case a NA is returned if(singlevaluecvzero=FALSE) 
# else a value of 0 is returned.
	z <- x[!is.na(x)]
	if (length(z) == 0) { return(NA) 
	} else if (na.rm == FALSE & length(z) < length(x)) { return(NA)	 
	} else if (length(z) == 1 & singlevalueaszero == TRUE) { return(0)
	} else {
		return(100 * sd(z) / mean(z))
	}	
}
