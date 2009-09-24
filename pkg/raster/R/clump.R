# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : May 2009
# Version 0.9
# Licence GPL v3

clump <- function(raster, filename=NULL, ...) {

	warning('clump function is under development; results are approximate')

	datatype <- .datatype(...)
	filetype <- .filetype(...)
	overwrite <- .overwrite(...)
	progress <- .progress(...)

	if (is.null(filename)) { filename <- "" }
	if (filename != ""  & file.exists(filename) & overwrite==FALSE) {
		stop("file exists. Use another name or 'overwrite=TRUE' if you want to overwrite it")
	}
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
	
	starttime <- proc.time()
	pb <- .setProgressBar(nrow(x1), type = progress)

	for (r in 1:nrow(x1)) {
		c1 <- c2
		c2[] <- 0
		b <- getValues(raster, r)
		b <- which(b > 0)

		for ( cc in b ) {
			if (isTRUE(c2[cc-1] > 0)) { 
				c2[cc] <- c2[cc-1] 
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
	# check for joining clumps:
		for ( cc in b ) {
			if (isTRUE(c1[cc] > 0) & isTRUE(c1[cc] != c2[cc])) {
				rcl <- rbind(rcl, c(c2[cc], c1[cc]))
			}
			if (isTRUE(c1[cc-1] > 0) & isTRUE(c1[cc-1] != c2[cc])) {
				rcl <- rbind(rcl, c( c2[cc], c1[cc-1] ))
			}
			if (isTRUE(c1[cc+1] > 0) & isTRUE(c1[cc+1] != c2[cc])) {
				rcl <- rbind(rcl, c(c2[cc], c1[cc+1] ))
			}
		}
	
		if (tmpfile1 == "") {
			v <- c(v, c2)
		} else {
			x1 <- setValues(x1, c2, r)
			x1 <- writeRaster(x1)
		}	
		
		rcl <- unique(rcl)
		.doProgressBar(pb, r) 			

	}
	.closeProgressBar(pb, starttime)

	if (tmpfile1 == "") {
		x1 <- setValues(x1, v)
	}
	
	if (nrow(rcl) > 1) {
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
	
		rclcomb <- rbind(rcldown, rclup, c(0, NA))
		rclm <- cbind(rclcomb[,1], rclcomb)
	} else {
		rclm <- c(0, 0, NA)
	}
	x2 <- reclass(x1, rclm, update=TRUE, filename=filename, datatype=datatype, overwrite=overwrite, progress=progress)

	if (tmpfile1 != "") { 	
		removeRasterFile(x1) 
	}
	# return(list(x1, x2, rcl1, rclm))
	return(x2)
}	


