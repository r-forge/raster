# Author: Jacob van Etten jacobvanetten@yahoo.com
# International Rice Research Institute
# Date :  January 2009
# Version 1.0
# Licence GPL v3

#setGeneric("distance", function(object, ...) standardGeneric("distance"))

#setMethod("distance", signature(object = "RasterLayer"), def =	
	
distance <-	function(object, filename="") {
		n <- ncell(object)
		
		if(dataContent(object)=='all'){
			fromCells <- which(!is.na(values(object)))
			toCells <- which(is.na(values(object)))
			accDist <- rep(0,times=n)
			accDist[toCells] <- Inf
			if (isLatLon(object)) {
				while(length(fromCells)>0)
				{			
					adj <- adjacency(object,fromCells=fromCells,toCells=toCells,directions=8)
					coord <- cbind(xyFromCell(object,adj[,1]),xyFromCell(object,adj[,2]))
					distance <- apply(coord,1,function(x){distanceGreatcircle(x[1:2],x[3:4])})
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
			outRaster <- object
			outRaster <- setValues(outRaster, accDist)	
			return(outRaster)
		}
		if( dataSource(object) =='disk'){ #to be tested
		
		# Fix error:  startRow has not been initialized.
			stop("currently only available for dataContent(raster)=='all'; use readAll")
			
			nrows <- nrow(object)
			ncols <- ncol(object)
			outRaster <- setRaster(object, filename)
			for(r in 1:nrows)
			{
				rowValues <- valuesRow(readRow(object, rownr = r), rownr = r)
				outRowValues <- rep(Inf,times=ncols)
				outRowValues[is.na(rowValues)] <- 0
				outRaster <- setValues(outRaster, outRowValues, r)
				outRaster <- writeRaster(outRaster, overwrite=TRUE)				
			}
			if(isLatLon(object)){
				remainingCells <- TRUE
				while(remainingCells){
					remainingCells <- FALSE
					oldRowValues <- integer(0)
					rowWindow <- valuesRow(readRow(outRaster, rownr=1), rownr = 1)
					for(r in 1:nrows){
						if(r<nrows-1){rowWindow <- c(rowWindow,valuesRow(readRow(outRaster, rownr=r+1), rownr=r+1))}
						adj <- adjacency(fromCells=(((max(1,r-1))*ncols)+1):(min(nrows,(r+2)*ncols)), toCells=((r-1)*ncols+1):(r*ncols),directions=8)
						coord <- cbind(xyFromCell(object,adj[,1]),xyFromCell(object,adj[,2]))
						distance <- apply(coord,1,function(x){distanceGreatcircle(x[1:2],x[3:4])})
						adj <- adj-((r-1)*ncols+1)
						transitionValues <- as.vector(rowWindow)[adj[,1]] + distance
						transitionValues <- tapply(transitionValues,adj[,2],min)
						transitionValues <- transitionValues[transitionValues < Inf]
						index <- as.integer(names(transitionValues))
						newValues <- pmin(transitionValues,rowWindow[index])
						if(newValues != rowWindow[index]){remainingCells<-TRUE}
						rowWindow[index] 
						# startRow is undefined
						# outRaster <- setValues(outRaster, rowValues, startRow)
						outRaster <- writeRaster(outRaster, overwrite=TRUE)
						if(r>1){rowWindow <- rowWindow[-1:ncols]}
					}
				}
			}
			else{
				remainingCells <- TRUE
				while(remainingCells){
					remainingCells <- FALSE
					oldRowValues <- integer(0)
					rowWindow <- valuesRow(readRow(outRaster, rownr = 1), rownr = 1)
					for(r in 1:nrows){
						if(r<nrows-1){rowWindow <- c(rowWindow,valuesRow(readRow(outRaster, rownr=r+1), rownr=r+1))}
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
						if(newValues != rowWindow[index]){remainingCells<-TRUE}
						rowWindow[index] 
						# startRow is undefined
						#outRaster <- setValues(outRaster, rowValues, startRow)
						outRaster <- writeRaster(outRaster, overwrite=TRUE)
						if(r>1){rowWindow <- rowWindow[-1:ncols]}
					}
				}
			}
		}
	}
#)