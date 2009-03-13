# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3

setMethod('hist', signature(x='RasterStack'), 
	function(x, layer=1, maxsamp=10000, ...) {
		x <- asRasterLayer(x, layer)
		callNextMethod(x, maxsamp=10000, ...)
	}
)

setMethod('hist', signature(x='Raster'), 
	function(x, maxsamp=10000, ...){
		if (dataContent(x) != 'all') {
			if (dataSource(x) == 'disk') {
		# TO DO: make a function that does this by block and combines  all data into a single histogram

				if (ncell(x) <= maxsamp) {
					values <- na.omit(values(readAll(x)))
				} else {
					values <- sampleRandom(x, maxsamp)
					msg <- paste(round(100 * maxsamp / ncell(x)), "% of the raster cells were used", sep="")
					if (maxsamp > length(values)) {
						msg <- paste(msg, " (of which ", 100 - round(100 * length(values) / maxsamp ), "% were NA)", sep="")
					}
					msg <- paste(msg, ". ",length(values)," values used.", sep="")
					warning(msg)
				}	
			} else { stop('cannot make a histogram; need data on disk or in memory')}
		} else {
			values <- values(x)
		}			
		hist(values, ...)
	}	
)
