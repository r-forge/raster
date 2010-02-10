# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.9
# Licence GPL v3

setMethod('hist', signature(x='RasterStackBrick'), 
	function(x, layer, maxsamp=10000, plot=TRUE, main, mfrow, ...) {
		
	
		if (missing(layer)) y = 1:nlayers(x)
		else if (is.character(layer)) {
			yy = NULL
			for (i in 1:length(y)) {
				yy = c(yy, which(layerNames(x) == y[i])[1])
			}
			y = yy
		} else { 
			y = layer 
		}
		y <- unique(as.integer(round(y)))
		y = na.omit(y)
		y = subset(y, y >= 1 & y <= nlayers(x))
		nl <- length(y)
		
		if (nl == 0) {stop('no existing layers selected')}
		
		if (nl > 1)	{
			res=list()
			if (nl > 16) {
				warning('only the first 16 layers are plotted')
				nl <- 16
				y <- y[1:16]
			}
			if (missing(main)) {	main=layerNames(x) }

			nc <- ceiling(sqrt(nl))
			nr <- ceiling(nl / nc)
			
			mfrow = par("mfrow")
			spots = mfrow[1] * mfrow[2]
			if (spots < nl) {
				par(mfrow=c(nr, nc))
			}
			for (i in 1:length(y)) {	
				r <- raster(x, index=y[i])
				m <- main[y[i]]
				if (plot) { res[[i]] = hist(r, maxsamp=maxsamp, main=m, ...)
				} else  { res[[i]] = hist(r, maxsamp=maxsamp, plot=FALSE, ...) }
			}		

		} else if (nl==1) {
			if (missing(main)) main = layerNames(x)[y]
			x <- raster(x, y)
			if (plot) { res = hist(x, maxsamp=maxsamp, main=main, ...)
			} else { res = hist(r, maxsamp=maxsamp, plot=FALSE, ...) }
		}
		
		if (plot) return(invisible(res))
		else return(res)
		
	}
)


setMethod('hist', signature(x='RasterLayer'), 
	function(x, layer=1, maxsamp=10000, main=NA,  plot=TRUE, ...){
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
		
		if (plot) return(invisible(res))
		else return(res)
	}	
)
