# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3

setMethod('hist', signature(x='RasterStackBrick'), 
	function(x, layer=0, maxsamp=100000, main='', ...) {
		layer <- round(layer)
		
		if (layer < 1) {
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
				if (length(main) == nl) {
					m <- main[nl]
				} else {
					m <- main
				}
				hist(r, maxsamp=maxsamp, main=m, ...)
			}		
		} else {
			if (layer > nlayers(hist)) {
				stop('layer number too high')
			}
			x <- raster(x, layer)
			hist(x, maxsamp=maxsamp, main=main, ...)
		}
	}
)


setMethod('hist', signature(x='RasterLayer'), 
	function(x, maxsamp=10000, main='', ...){
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
				msg <- paste(msg, ". ",length(values)," values used.", sep="")
				warning(msg)
			}	
		} else { 
			stop('cannot make a histogram; need data on disk or in memory')
		}		
		if (.shortDataType(x) == 'LOG') {
			values <- values * 1
		}
		hist(values, main=main, ...)
	}	
)
