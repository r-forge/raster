# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : May 2009
# Version 0.8
# Licence GPL v3

clump <- function(raster, filename=NULL, overwrite=FALSE, filetype='raster', datatype='INT4S', track=-1) {
	cat('WARNING: this function does not return correct results', '\n')
	
	if (is.null(filename)) { filename <- "" }
	tmpfile1 <- ""
	x1 <- raster(raster)
	dataType(x1) <- datatype
	if (!canProcessInMemory(x1, 3)) {
		tmpfile1 <- rasterTmpFile()
		filename(x1) <- tmpfile1
		if (filename == "") {
			filename <- rasterTmpFile()
			if (getOption('verbose')) { cat('writing raster to:', filename)	}								
		}
	}

	nc <- ncol(x1)
	v <- vector(length=0)
	nextclump <- 1
	c2 <- vector(length=nc)
	c2[] <- 0
	rcl <- matrix(NA, nrow=0, ncol=2)
	for (r in 1:nrow(x1)) {
		b <- valuesRow(raster, r)
		c1 <- c2
		c2[] <- 0
		for (cc in 1:nc) {
			if (b[cc]==1) {
				if (isTRUE(c2[cc-1] > 0)) { c2[cc] <- c2[cc-1] 
				} else if ( isTRUE(c1[cc-1] > 0) ) {
					c2[cc] <- c1[cc-1]
				} else if ( c1[cc] > 0) {
					c2[cc] <- c1[cc]
				} else if (isTRUE(c1[cc+1] > 0)) {
					c2[cc] <- c1[cc+1]
				} else {
					c2[cc] <- nextclump
					nextclump <- nextclump + 1					
				}
			}
		}

# check for joining clumps:
		
		for (cc in 1:nc) {
			if (c2[cc] > 0) {
				if (isTRUE(c1[cc] > 0) & isTRUE(c1[cc] != c2[cc])) {
					rcl <- rbind(rcl, c(c2[cc], c1[cc]))
				}
				if (isTRUE(c1[cc-1] > 0) & isTRUE(c1[cc-1] != c2[cc])) {
					rcl <- rbind(rcl, c(c2[cc], c1[cc-1] ))
				}
				if (isTRUE(c1[cc+1] > 0) & isTRUE(c1[cc+1] != c2[cc])) {
					rcl <- rbind(rcl, c(c2[cc], c1[cc+1] ))
				}
			}
		}
	
		if (tmpfile1 == "") {
			v <- c(v, c2)
		} else {
			x1 <- setValues(x1, c2)
			x1 <- writeRaster(x1, r)
		}	
	}
	if (tmpfile1 == "") {
		x1 <- setValues(x1, v)
	}
	rcl1 <- unique(rbind(rcl, cbind(rcl[,2], rcl[,1])))
	rcl <- rcl1[rcl1[,1] > rcl1[,2],]
	aggrcl1 <- aggregate(rcl, by=list(rcl[,1]), FUN=min)[,-1]
	colnames(rcl) <- c('a', 'b')
	colnames(aggrcl1) <- c('a', 'c')
	aggrcl2 <- merge(rcl, aggrcl1)[,-1]
	aggrcl2 <- aggrcl2[aggrcl2[,1] != aggrcl2[,2],]
	colnames(aggrcl2)[1] <- 'a'
	aggrcl <- rbind(aggrcl1, aggrcl2)
	aggrcl <- aggregate(aggrcl, by=list(aggrcl[,1]), FUN=min)[,-1]
	rcldown <- aggrcl[rev(order(aggrcl[,1])), ]
	
	rcl <- rcl1[rcl1[,1] < rcl1[,2],]
	aggrcl1 <- aggregate(rcl, by=list(rcl[,1]), FUN=max)[,-1]
	colnames(rcl) <- c('a', 'b')
	colnames(aggrcl1) <- c('a', 'c')
	aggrcl2 <- merge(rcl, aggrcl1)[,-1]
	aggrcl2 <- aggrcl2[aggrcl2[,1] != aggrcl2[,2],]
	colnames(aggrcl2)[1] <- 'a'
	aggrcl <- rbind(aggrcl1, aggrcl2)
	aggrcl <- aggregate(aggrcl, by=list(aggrcl[,1]), FUN=max)[,-1]
	rclup <- aggrcl[order(aggrcl[,1]), ]
	
	rclcomb <- rbind(rcldown, rclup)
	
	rclm <- cbind(rclcomb[,1], rclcomb)
	if (tmpfile1 == "") {
		x1 <- reclass(x1, rclm, update=TRUE, filename=filename, datatype=datatype, overwrite=overwrite)
		return(x1)
	} else {
		x2 <- reclass(x1, rclm, update=TRUE, filename=filename, datatype=datatype, overwrite=overwrite)
		removeRasterFile(x1)
		return(x2)
	}
}	

