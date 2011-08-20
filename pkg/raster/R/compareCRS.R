# author Robert Hijmans
# June 2010
# version 1.0
# license GPL3


.compareCRS <- function(x, y, unknown=FALSE) {
	
	step1 <- function(z) {
		if (class(z) == 'character') { 	# do nothing
		} else {
			z <- projection(z)
		}
		z <- gsub(' ', '', z)
		z <- unlist( strsplit(z, '+', fixed=TRUE) )[-1]
		do.call(rbind, strsplit(z, '='))
	}
	
	a <- step1(x)
	b <- step1(y)
	if (length(a) == 0 | length(b) == 0) {
		if (unknown) {
			return(TRUE)
		} else {
			return(FALSE) 
		}
	}
	i <- which(a[,1] %in% b[,1])
	j <- which(b[,1] %in% a[,1])
	a <- a[i, ,drop=FALSE]
	b <- b[j, ,drop=FALSE]
	a <- a[order(a[,1]), ,drop=FALSE]
	b <- b[order(b[,1]), ,drop=FALSE]
	if (! all(a[,2] == b[,2])) {
		return(FALSE)
	}
	return(TRUE)
}

