
if ( !isGeneric("corr") ) {
	setGeneric("corr", function(x, y, ...)
		standardGeneric("corr"))
}


setMethod('corr', signature(x='RasterLayer', y='RasterLayer'), 
	function(x, y,...) {
	
		compareRaster(x, y, crs=FALSE, stopiffalse=TRUE)
		
		if (canProcessInMemory(x, 8)) {
			s <- na.omit(getValues(stack(x, y)))
			cor(s[,1], s[,2])
		} else {
			s <- stack(x,y)
			msk <- sum(s, na.rm=FALSE)
			s <- mask(s, msk)
			x <- s[[1]]
			y <- s[[2]]
			mx <- cellStats(x, mean)
			my <- cellStats(y, mean)
			sx <- cellStats(x, sd)
			sy <- cellStats(y, sd)
			cellStats(((x - mx) * (y - my)) / (sx * sy), sum)/ (ncell(x)-1)
		}
	}
)


setMethod('corr', signature(x='RasterStackBrick', y='missing'), 
	function(x, y, ...) {
		n <- nlayers(x)
		if (n < 2) return(1)
		if (canProcessInMemory(x, nlayers(x)*4)) {
			s <- na.omit(getValues(x))
			s <- cor(s)
		} else {
			msk <- sum(x, na.rm=FALSE)
			x <- mask(x, msk)
			mx <- cellStats(x, mean)
			sx <- cellStats(x, sd)
			nc <- ncell(x)
			s <- matrix(NA, nrow=n, ncol=n)
			for (i in 1:(n-1)) {
				for (j in (i+1):n) {
					s[j,i] <- s[i,j] <- cellStats(((x[[i]] - mx[i]) * (x[[j]] - mx[j])) / (sx[i] * sx[j]), sum)/ (nc-1)
				}
			}
			diag(s) <- 1			
		}
		colnames(s) <- rownames(s) <- names(x)
		s		
	}
)



