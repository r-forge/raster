# Author: Jacob van Etten and Robert J. Hijmans
# email jacobvanetten@yahoo.com
# Date :  January 2009
# Version 0.9
# Licence GPL v3

#setGeneric("distance", function(object, ...) standardGeneric("distance"))

#setMethod("distance", signature(object = "RasterLayer"), def =	

gridDistance <- function(object, filename="", ...) {

	datatype <- .datatype(...)
	filetype <- .filetype(...)
	overwrite <- .overwrite(...)
	inMemory <- .inMemory(...)
#	inMemory=TRUE
		
		n <- ncell(object)

		if ((dataContent(object) != 'all') & (dataSource(object) != 'disk')) {
			stop('cannot compute distance on a RasterLayer with no data')
		}
		
		if((inMemory) & (canProcessInMemory(object, 5))){
			if (dataContent(object) != 'all' ) { 
				object <- readAll(object) 
			}
			outRaster <- raster(object, filename=filename)

			fromCells <- which(!is.na(values(object)))
			fromCells <- fromCells[which(values(object)[fromCells] == TRUE)]
			toCells <- which(is.na(values(object)))
			accDist <- rep(0,times=n)
			accDist[toCells] <- Inf
			if (isLatLon(object)) {
				while(length(fromCells)>0) {			
					adj <- adjacency(object,fromCells=fromCells,toCells=toCells,directions=8)
					coord <- cbind(xyFromCell(object,adj[,1]),xyFromCell(object,adj[,2]))
					distance <- apply(coord,1,function(x){pointDistance(x[1:2],x[3:4], type='GreatCircle')})
					#What follows is the same as for  projected  data ( further below)
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
				outRaster <- writeRaster(outRaster, filename)
			}
			return(outRaster)
			
			
		} else { 
		
			stop('not yet implemented for large rasters')
			
			nrows <- nrow(object)
			ncols <- ncol(object)

			m <- c(-Inf, Inf, 0)
			r1 <- reclass(object, m, filename=rasterTmpFile(), overwrite=TRUE)
			r2 <- raster(r1, rasterTmpFile())
			
			if(isLatLon(object)){
				remainingCells <- TRUE
				while (remainingCells) {
					remainingCells <- FALSE
					r1 <- readRow(r1, rownr=1)
					rowWindow <- values(r1)
					for(r in 1:nrows){
						if(r < nrows-1) {
							r1 <- readRow(r1, rownr=r+1)
							rowWindow <- c(rowWindow, values(r1))
						}
						adj <- adjacency(object, fromCells=(((max(1,r-1))*ncols)+1):(min(nrows,(r+2)*ncols)), toCells=((r-1)*ncols+1):(r*ncols),directions=8)
						coord <- cbind(xyFromCell(object,adj[,1]),xyFromCell(object,adj[,2]))
						distance <- apply(coord,1,function(x){pointDistance(x[1:2],x[3:4], type='GreatCircle')})
						adj <- adj-((r-1)*ncols+1)
						transitionValues <- as.vector(rowWindow)[adj[,1]] + distance
						transitionValues <- tapply(transitionValues,adj[,2],min)
						transitionValues <- transitionValues[transitionValues < Inf]
						index <- as.integer(names(transitionValues))
						newValues <- pmin(transitionValues, rowWindow[index])
						if (sum(is.na(newValues)) > 0) {
							remainingCells<-TRUE
						} else if(newValues != rowWindow[index]){ 
							remainingCells<-TRUE
						}						
						r2 <- setValues(r2, newValues, r)
						r2 <- writeRaster(r2, filename(r2), overwrite=TRUE)
						if(r > 1){
							rowWindow <- rowWindow[-1:ncols]
						}
					} 
					rtmp <- r1
					r1 <- r2
					r2 <- rtmp
				}
			} else {
				remainingCells <- TRUE
				while(remainingCells){
					remainingCells <- FALSE
					r1 <- readRow(r1, rownr=1)
					rowWindow <- values(r1)
					for(r in 1:nrows){
						if(r < nrows-1){
							r1 <- readRow(r1, rownr=r+1)
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
						r2 <- setValues(r2, newValues, r)
						r2 <- writeRaster(r2,  filename(r2), overwrite=TRUE)
						if(r > 1){
							rowWindow <- rowWindow[-1:ncols]
						}
					}
					rtmp <- r1
					r1 <- r2
					r2 <- rtmp
				}
			}
			outRaster <- saveAs(r1, filename, overwrite=overwrite, filetype=filetype, datatype=datatype)
			removeRasterFile(r1)
			removeRasterFile(r2)
		}
	}
#)
