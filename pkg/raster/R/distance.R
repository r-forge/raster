# Author: Jacob van Etten jacobvanetten@yahoo.com
# International Rice Research Institute
# Date :  January 2009
# Version 1.0
# Licence GPL v3

setGeneric("distance", function(object, ...) standardGeneric("distance"))

setMethod("distance", signature(object = "RasterLayer"), def =	function(object, filename="") {
		n <- ncell(object)
		
		if(dataContent=='all'){
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
		if(dataContent(object)=='disk'){ #to be tested
			nrows <- nrow(object)
			ncols <- ncol(object)
			outRaster <- setRaster(object, filename)
			for(r in 1:nrows)
			{
				rowValues <- readRows(object, startrow = r, nrows = 1)
				outRowValues <- rep(Inf,times=ncols)
				outRowValues[is.na(rowValues)] <- 0
				outRaster <- setValues(outRaster, outRowValues, r)
				outRaster <- writeRaster(outRaster, overwrite=overwrite)				
			}
			if(isLatLon){
				while(remainingCells){
					remainingCells <- FALSE
					oldRowValues <- integer(0)
					for(r in 0:(nrows-1))){
						startRow <- max(r,1)
						endRow <- min(r+1,nrows)
						startCell <- ((startRow - 1) * ncols) + 1 
						endCell <- endRow * ncols

						rowValues <- cbind(oldRowValues,readRows(outRaster, startrow=startRow, nrows=1))
						fromCells <- which(rowValues < Inf) + startCell - 1

						toCells <-  startCell : endCell 
						adj <- adjacency(object, fromCells=fromCells, toCells= toCells1, directions=8) #optimize adjacency by allowing row argument
						coord <- cbind(xyFromCell(object,adj[,1]),xyFromCell(object,adj[,2]))
						distance <- apply(coord,1,function(x){distanceGreatcircle(x[1:2],x[3:4])})

						transitionValues <- rowValues[adj[,1]-(startCell-1)] + distance
						transitionValues <- tapply(c(rowValues,transitionValues),c(toCells,adj[,2]-(startCell-1)),min)
						transitionValues <- transitionValues[transitionValues < Inf]
						index <- as.integer(names(transitionValues))
						rowValues[index] <- transitionValues						
												
						outRaster <- setValues(outRaster, rowValues, startRow)
						outRaster <- writeRaster(outRaster)
						
						oldRowValues <- rowValues[(length(rowValues)-ncols+1):length(rowValues)]
						if(length(fromCells)>0){remainingCells <- TRUE}
					}
				}
			}
			else{
			
			}
		}
	}
)

outRaster <- setRaster(x, filename)

			if (filename(outRaster) == "") {
				v <- c(v, vals)
			} else {
				outRaster <- setValues(outRaster, vals, r)
				outRaster <- writex(outRaster, overwrite=overwrite)
			}
		} 
		if (filename(outRaster) == "") { 
			outRaster <- setValues(outRaster, v) 
		}
