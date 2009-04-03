# Author: Jacob van Etten jacobvanetten@yahoo.com
# International Rice Research Institute
# Date :  January 2009
# Version 1.0
# Licence GPL v3

#setGeneric("distance", function(object, ...) standardGeneric("distance"))

#setMethod("distance", signature(object = "RasterLayer"), def =	
	
distance <-	function(object, filename="", filetype='raster', overwrite=FALSE, datatype='FLT4S') {
		n <- ncell(object)
#		if (dataSource = 'disk' & dataContent(object)=='all' & canProcessInMemory(object, 6)) {
#			object <- readAll(object)
#		}
		
		if(dataContent(object)=='all' & canProcessInMemory(object, 5)){
			outRaster <- raster(object, filename=filename)

			fromCells <- which(!is.na(values(object)))
			toCells <- (1:n)[-fromCells]
			accDist <- rep(0,times=n)
			accDist[toCells] <- Inf
			if (isLatLon(object)) {
				while(length(fromCells)>0) {			
					adj <- adjacency(object,fromCells=fromCells,toCells=toCells,directions=8)
					coord <- cbind(xyFromCell(object,adj[,1]),xyFromCell(object,adj[,2]))
					distance <- apply(coord,1,function(x){pointDistance(x[1:2],x[3:4], type='GreatCircle')})
					#What follows is the same as for  non-projected (below)
					transitionValues <- accDist[adj[,1]] + distance
					transitionValues <- tapply(transitionValues,adj[,2],min)
					transitionValues <- transitionValues[transitionValues < Inf]
					index <- as.integer(names(transitionValues))
					fromCells <- index[transitionValues < accDist[index]]
					accDist[index] <- pmin(transitionValues,accDist[index])
				}
			} else {
				while(length(fromCells)>0) {			
					adj1 <- adjacency(object,fromCells=fromCells,toCells=toCells,directions=4)
					adj2 <- adjacency(object,fromCells=fromCells,toCells=toCells,directions="Bishop")
					distance <- c(rep(1,length=length(adj1[,1])),rep(sqrt(2),length=length(adj2[,1])))
					adj <- rbind(adj1,adj2)
					#What follows is the same as for LatLon
					transitionValues <- accDist[adj[,1]] + distance
					transitionValues <- tapply(transitionValues,adj[,2],min)
					transitionValues <- transitionValues[transitionValues < Inf]
					index <- as.integer(names(transitionValues))
					fromCells <- index[transitionValues < accDist[index]]
					accDist[index] <- pmin(transitionValues,accDist[index])
				}
			}
			outRaster <- setValues(outRaster, accDist)	
			if (filename != "") {
				outRaster <- writeRaster(outRaster)
			}
			return(outRaster)
			
		} else if( dataSource(object) =='disk'){ 
		
			stop('not yet implemented for data on disk')
			
			nrows <- nrow(object)
			ncols <- ncol(object)

			m <- c(-Inf, Inf, 0)
			rsl1 <- reclass(object, m, filename=tempfile(), overwrite=TRUE)
			rsl2 <- raster(tempfile())
			
			if(isLatLon(object)){
				remainingCells <- TRUE
				while (remainingCells) {
					remainingCells <- FALSE
					rsl1 <- readRow(rsl1, rownr=1)
					rowWindow <- values(rsl1)
					for(r in 1:nrows){
						if(r < nrows-1) {
							rsl1 <- readRow(rsl1, rownr=r+1)
							rowWindow <- c(rowWindow, values(rsl1))
						}
						adj <- adjacency(fromCells=(((max(1,r-1))*ncols)+1):(min(nrows,(r+2)*ncols)), toCells=((r-1)*ncols+1):(r*ncols),directions=8)
						coord <- cbind(xyFromCell(object,adj[,1]),xyFromCell(object,adj[,2]))
						distance <- apply(coord,1,function(x){pointDistance(x[1:2],x[3:4], type='GreatCircle')})
						adj <- adj-((r-1)*ncols+1)
						transitionValues <- as.vector(rowWindow)[adj[,1]] + distance
						transitionValues <- tapply(transitionValues,adj[,2],min)
						transitionValues <- transitionValues[transitionValues < Inf]
						index <- as.integer(names(transitionValues))
						newValues <- pmin(transitionValues,rowWindow[index])
						if(newValues != rowWindow[index]){ 
							remainingCells<-TRUE
						}						
						rsl2 <- setValues(rsl2, newValues, r)
						rsl2 <- writeRaster(rsl2, overwrite=TRUE)
						if(r > 1){
							rowWindow <- rowWindow[-1:ncols]
						}
					} 
					rtmp <- rsl1
					rsl1 <- rsl2
					rsl2 <- rtmp
				}
			} else {
				remainingCells <- TRUE
				while(remainingCells){
					remainingCells <- FALSE
					rsl1 <- readRow(rsl1, rownr=1)
					rowWindow <- values(rsl1)
					for(r in 1:nrows){
						if(r < nrows-1){
							rsl1 <- readRow(rsl1, rownr=r+1)
							rowWindow <- c(rowWindow, values(outRaster))
						}
						fromCells <- (((max(1,r-1))*ncols)+1):(min(nrows,(r+2)*ncols))
						toCells <- ((r-1)*ncols+1):(r*ncols)
						adj1 <- adjacency(object,fromCells=fromCells,toCells=toCells,directions=4)
						adj2 <- adjacency(object,fromCells=fromCells,toCells=toCells,directions="Bishop")
						distance <- c(rep(1,length=length(adj1[,1])),rep(sqrt(2),length=length(adj2[,1])))
						adj <- rbind(adj1,adj2)
						adj <- adj-((r-1)*ncols+1)
						transitionValues <- as.vector(rowWindow)[adj[,1]] + distance
						transitionValues <- tapply(transitionValues,adj[,2],min)
						transitionValues <- transitionValues[transitionValues < Inf]
						index <- as.integer(names(transitionValues))
						newValues <- pmin(transitionValues,rowWindow[index])
						if(newValues != rowWindow[index]){
							remainingCells<-TRUE
						}
						rsl2 <- setValues(rsl2, newValues, r)
						rsl2 <- writeRaster(rsl2, overwrite=TRUE)
						if(r > 1){
							rowWindow <- rowWindow[-1:ncols]
						}
					}
					rtmp <- rsl1
					rsl1 <- rsl2
					rsl2 <- rtmp
				}
			}
			outRaster <- saveAs(rsl1, filename, overwrite=overwrite, filetype=filetype, datatype=datatype)
		}
	}
#)
