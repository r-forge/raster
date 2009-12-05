# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : May 2009
# Version 0.9
# Licence GPL v3


	
if (!isGeneric("clump")) {
	setGeneric("clump", function(x, ...)
		standardGeneric("clump"))
}	

setMethod('clump', signature(x='RasterLayer'), 
function(x, filename='', ...) {

	if (filename != ""  & file.exists(filename)) {
		if (.overwrite(...)==FALSE) {
			stop("file exists. Use another name or 'overwrite=TRUE' if you want to overwrite it")
		}
	}

	x1 <- raster(x)
	tmpfile <- ""
	if (!canProcessInMemory(x1, 3)) {
		tmpfile <- rasterTmpFile()
	}
	if (tmpfile=='') { 
		nc <- ncol(x1)
		nextclump <- 1
		v <- matrix(0, ncol=ncol(x1)+1, nrow=nrow(x1)+1)
		pb <- pbCreate(nrow(x1), type = .progress(...))
	
		for (r in 1:nrow(x1)) {
			rr <- r + 1
			b <- getValues(x, r)
			b <- which(b != 0)
			for ( cc in b ) {
				vv <- na.omit(c(v[(rr-1),(cc-1):(cc+1)], v[rr, cc-1]))
				vm <- max(vv)
				if (vm > 0) {  
					v[rr, cc] <- vm
					vvv <- vv[(vv > 0) & (vv < vm)]
					if (length(vvv) > 0) {
						vvv <- unique(vvv)
						for (i in vvv) {
							v[v==i] <- vm
						}
					}
				} else {
					v[rr, cc] <- nextclump
					nextclump <- nextclump + 1					
				}
			}
			pbStep(pb, r) 			
		}
		pbClose(pb)
		v[v==0] <- NA
		rm(x)
		x1 <- setValues(x1, as.vector(t(v[-1,-ncol(v)])))
		u <- na.omit(unique(x1))
		u <- cbind(u, u, 1:length(u))
		x1 <- reclass(x1, u, ...)
		return(x1)
	} else {
		nc <- ncol(x1)
		nextclump <- 1
		c2 <- vector(length=nc)
		c2[] <- 0
		rcl <- matrix(NA, nrow=0, ncol=2)
		atrcl <- matrix(NA, nrow=0, ncol=2)
		pb <- pbCreate(nrow(x1), type = .progress(...))
	
		for (r in 1:nrow(x1)) {
			c1 <- c2
			c2[] <- 0
			b <- getValues(x, r)
			b <- which(b != 0)
			trcl <- atrcl
			for ( cc in b ) {
				vv <- na.omit(c(c1[(cc-1):(cc+1)], c2[cc-1]))
				vm <- max(vv)
				if (vm > 0) {  
					c2[cc] <- vm
					vvv <- vv[(vv > 0) & (vv < vm)]
					if (length(vvv) > 0) {
						vvv <- unique(vvv)
						trcl <- rbind(trcl, cbind(vvv, vm))
						c1[c1==vvv] <- vm
					}
				} else {
					c2[cc] <- nextclump
					nextclump <- nextclump + 1					
				}
			}
			if (nrow(trcl) > 0) {
				for (i in 1:nrow(trcl)) {
					c2[c2==trcl[i,1]] <- trcl[i,2]
				}
			}	
		
			x1 <- setValues(x1, c2, r)
			x1 <- writeRaster(x1, filename=tmpfile, format='raster', datatype='INT4U')
				
			trcl <- unique(trcl)
			rcl <- unique(rbind(rcl, trcl))
			pbStep(pb, r) 			
		}
		pbClose(pb)

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
			rcldown <- aggrcl[rev(order(aggrcl[,2])), ]
		
			rcl <- rcl1[rcl1[,1] < rcl1[,2],]
			aggrcl1 <- aggregate(rcl, by=list(rcl[,1]), FUN=max)[,-1]
			colnames(rcl) <- c('a', 'b')
			colnames(aggrcl1) <- c('a', 'c')
			aggrcl2 <- merge(rcl, aggrcl1)[,-1]
			aggrcl2 <- aggrcl2[aggrcl2[,1] != aggrcl2[,2],]
			colnames(aggrcl2)[1] <- 'a'
			aggrcl <- rbind(aggrcl1, aggrcl2)
			aggrcl <- aggregate(aggrcl, by=list(aggrcl[,1]), FUN=max)[,-1]
			rclup <- aggrcl[order(aggrcl[,2]), ]
	
			rclcomb <- rbind(rcldown, rclup, c(0, NA))
			rclm <- cbind(rclcomb[,1], rclcomb)
		} else if (nrow(rcl) == 1) {
			rclcomb <- rbind(rcl, c(0, NA))
			rclm <- cbind(rclcomb[,1], rclcomb)
		} else {
			rclm <- c(0, 0, NA)
		}

		x1 <- reclass(x1, rclm, update=TRUE, filename=rasterTmpFile(), progress=.progress(...))
		u <- na.omit(unique(x1))
		u <- cbind(u, u, 1:length(u))
		x1 <- reclass(x1, u, ...)
		return(x1)
	}
}
)

