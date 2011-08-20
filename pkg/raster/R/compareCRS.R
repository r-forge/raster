# author Robert Hijmans
# June 2010
# version 1.0
# license GPL3


.compareCRS <- function(x, y) {
	if (class(x) == 'character') { 
	} else if (class(x) == 'CRS') { 
		x <- x@projargs 
	} else {
		x <- projection(x) 
	}
	if (class(y) == 'character') { 
	} else if (class(y) == 'CRS') { 
		y <- y@projargs 
	} else {
		y <- projection(y)
	}
	x <- gsub(' ', '', x)
	y <- gsub(' ', '', y)
	x <- unlist( strsplit(x, '+', fixed=TRUE) )[-1]
	y <- unlist( strsplit(y, '+', fixed=TRUE) )[-1]
	a <- do.call(rbind, strsplit(x, '='))
	b <- do.call(rbind, strsplit(y, '='))
	i <- which(a[,1] %in% b[,1])
	j <- which(b[,1] %in% a[,1])
	a <- a[i, ]
	b <- b[j, ]
	a <- a[order(a[,1]),]
	b <- b[order(b[,1]),]
	if (! all(a[,2] == b[,2])) {
		return(FALSE)
	}
	return(TRUE)
}

