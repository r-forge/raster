# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,1
# Licence GPL v3


#UNDER DEVELOPMENT
.polygon.to.raster <- function(sppoly, raster) {
# check if bbox of raster and sppoly overlap
	spbb <- bbox(sppoly)
	rsbb <- bbox(raster)
	if (spbb[1,1] > rsbb[1,2] | spbb[2,1] > rsbb[2,2]) {
		stop('polygon and raster have no overlapping areas')
	}
	npol <- length(sppoly@polygons)
	info <- matrix(NA, nrow=npol, ncol=3)
	for (i in 1:npol) {
		info[i,1] <- length(sppoly@polygons[[i]]@Polygons)
		miny <- NULL
		maxy <- NULL
		for (j in 1:info[i,1]) {
			miny <- min(miny, min(sppoly@polygons[[i]]@Polygons[[j]]@coords[,2]))
			maxy <- max(maxy, max(sppoly@polygons[[i]]@Polygons[[j]]@coords[,2]))
		}
		info[i,2] <- miny
		info[i,3] <- maxy
	}
	lxmin <- min(spbb[1,1], rsbb[1,1]) - xres(raster)
	lxmax <- max(spbb[1,2], rsbb[1,2]) + xres(raster)
	for (r in 1:nrow(raster)) {
		ly <- yFromRow(raster, r)
		uly <- ly + 0.01 * yres(raster)
		lly <- ly - 0.01 * yres(raster)
		for (i in 1:npol) {
			if (info[i,2] > uly | info[i,3] < lly) {
				# do nothing
			} else {
				for (j in 1:info[i,1]) {
					if ( max ( sppoly@polygons[[i]]@Polygons[[j]]@coords[,2] ) < ly  |  min( sppoly@polygons[[i]]@Polygons[[j]]@coords[,2] ) > ly ) {
						# do nothing
					} else {
						cds <- sppoly@polygons[[i]]@Polygons[[j]]@coords
						c1 <- rbind(cds[length(cds[,1])], cds)
						c2 <- rbind(cds, cds[1])
						cds <- cbind(c1, c2)
						intlines <- cds[cds[ (cds[,2] < ly & cds[,4] > ly ) | (cds[,2] > ly & cds[,4] < ly)] ,]
						print(intlines)
					}	
				}
			}
		}	
	}
	return(info)
}

#polygon.to.raster(nld, rs)

