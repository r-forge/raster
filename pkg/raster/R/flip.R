# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

flip <- function(raster, direction='y', filename='', ...)  {
	
	filename <- trim(filename)
	
	if (!canProcessInMemory(raster, 2) && filename == '') {
		filename <- rasterTmpFile()
		if (getOption('verbose')) { cat('writing raster to:', filename(outRaster))	}						
	}
	outRaster <- raster(raster, filename=filename)
	
	if (!(direction %in% c('y', 'x'))) {
		stop('directions should be y or x')
	}
		
	if ( filename == "" ) {
		if (dataContent( raster ) != 'all') {
			v <- values(readAll(raster), format='matrix')
		} else {
			v <- values(raster, format='matrix')
		}
		if (direction == 'h') {
			v <- v[nrow(v):1,]
		} else {
			v <- v[,ncol(v):1]
		}
		outRaster <- setValues(outRaster, as.vector(t(v)))
	} else {
	
		pb <- pbCreate(nrow(outRaster), type=.progress(...))

		if (direction == 'h') {
			for (r in nrow(raster):1) {
				res <- getValues(raster, r)
				nr <- nrow(outRaster) - r + 1
				outRaster <- setValues(outRaster, res, nr)
				outRaster <- writeRaster(outRaster, filename=filename, ...)
				pbStep(pb, r)
			}
		} else {
			for (r in 1:nrow(raster)) {
				res <- getValues(raster, r)
				outRaster <- setValues(outRaster, rev(res), r)
				outRaster <- writeRaster(outRaster, filename=filename, ...)
				pbStep(pb, r)
			}
		}
		pbClose(pb)
	}
	return(outRaster)
}

