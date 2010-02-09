# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.9
# Licence GPL v3

setMethod('hist', signature(x='RasterStackBrick'), 
	function(x, layer=0, maxsamp=10000, main=NA, plot=TRUE, ...) {
		layer <- round(layer)
		
		res=list()
		
		if (layer < 1) {
			if (is.na(main)) {
				main=layerNames(x)
			}
			nl <- nlayers(x)
			if (nl > 12) {
				warning('only first 12 layers are plotted')
				nl <- 12
			}
			nc <- ceiling(sqrt(nl))
			nr <- ceiling(nl / nc)
			par(mfrow=c(nr, nc))
			for (i in 1:nl) {	
				r <- raster(x, i)
				m <- main[i]
				if (plot) { res[[i]] = hist(r, maxsamp=maxsamp, main=m, ...)
				} else  { res[[i]] = hist(r, maxsamp=maxsamp, plot=FALSE, ...) }
			}		
		} else {
			if (layer > nlayers(x)) {
				stop('layer number too high')
			}
			x <- raster(x, layer)
			if (plot) { res[[1]] = hist(x, maxsamp=maxsamp, main=main, ...)
			} else { res[[1]] = hist(r, maxsamp=maxsamp, plot=FALSE, ...) }
		}
		
		return(invisible(res))
		
	}
)


setMethod('hist', signature(x='RasterLayer'), 
	function(x, maxsamp=10000, main=NA,  plot=TRUE, ...){
		if (dataContent(x) == 'all') {
			values <- values(x)
		} else if (dataSource(x) == 'disk') {
			
			if (ncell(x) <= maxsamp) {
				values <- na.omit(values(readAll(x)))
			} else {

			# TO DO: make a function that does this by block and combines  all data into a single histogram
				values <- sampleRandom(x, maxsamp)
				msg <- paste(round(100 * maxsamp / ncell(x)), "% of the raster cells were used", sep="")
				if (maxsamp > length(values)) {
					msg <- paste(msg, " (of which ", 100 - round(100 * length(values) / maxsamp ), "% were NA)", sep="")
				}
				warning( paste(msg, ". ",length(values)," values used.", sep="") )
			}	
		} else { 
			stop('cannot make a histogram; need data on disk or in memory')
		}		
		if (.shortDataType(x) == 'LOG') {
			values <- values * 1
		}
		if (plot) { res = hist(values, main=main, ...)  
		} else { res = hist(values, plot=FALSE, ...)  }
		
		return(invisible(res))
	}	
)
