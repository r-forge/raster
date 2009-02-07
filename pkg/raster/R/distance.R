# Author: Jacob van Etten jacobvanetten@yahoo.com
# International Rice Research Institute
# Date :  January 2009
# Version 1.0
# Licence GPL v3

setGeneric("distance", function(object, ...) standardGeneric("distance"))

setMethod("distance", signature(object = "RasterLayer"), def =	function(object) {
		n <- ncell(object)
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
				
				transitionValues <- accDist[adj[,1]] + distance
				transitionValues <- tapply(transitionValues,adj[,2],min)
				transitionValues <- transitionValues[transitionValues < Inf]
				index <- as.integer(names(transitionValues))
				fromCells <- index[transitionValues < accDist[index]]
				accDist[index] <- pmin(transitionValues,accDist[index])
			}
		}
		result <- object
		result <- setValues(result, accDist)	
		return(result)
	}
)
