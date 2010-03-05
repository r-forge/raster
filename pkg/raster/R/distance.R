# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric("distance")) {
	setGeneric("distance", function(x, ...)
		standardGeneric("distance"))
}	


setMethod('distance', signature(x='RasterLayer'), 

function(x, filename='', ...) {

	r = edge(x, classes=FALSE, type='inner', asNA=TRUE, progress=.progress(...)) 
	
	pts <- try(  rasterToPoints(r, fun=function(z){z>0})[,1:2] )
	if (class(pts) == "try-error") {
		return( .distanceRows(x, filename=filename, ...) )
	}
	if (nrow(pts) == 0) {
		stop('RasterLayer has no NA cells (for which to compute a distance)')
	}

	rst <- raster(x)
	
	if (.couldBeLonLat(rst)) { disttype <- 'GreatCircle' } else { disttype <- 'Euclidean' }
	                                                                        
	filename <- trim(filename)
	if (!canProcessInMemory(rst, 2) && filename == '') {
		filename <- rasterTmpFile()
		if (getOption('verbose')) { cat('writing raster to:', filename)	}						
	}
	xy <- xFromCol(rst, 1:ncol(rst))
	xy <- cbind(xy, NA)
	
	if (filename == '') {
		v <- matrix(ncol=nrow(rst), nrow=ncol(rst))
	} 
	
	pb <- pbCreate(nrow(rst), type=.progress(...))
	for (r in 1:nrow(rst)) {	
		vals <- getValues(x, r)
		i = which(is.na(vals))
		if (length(i) > 0) {
			xy[,2] <- yFromRow(rst, r)
			for (c in i) {
				vals[c] <- min( pointDistance(xy[c,], pts, type=disttype) )
			}
		}
		if (filename == "") {
			v[,r] <- vals
		} else {
			rst <- setValues(rst, vals, r)
			rst <- writeRaster(rst, filename=filename, ...)
		}
		pbStep(pb, r) 	
	}	
	pbClose(pb)
	
	if (filename == "") { 
		rst <- setValues(rst, as.vector(v)) 
	}
	return(rst)
}
)

