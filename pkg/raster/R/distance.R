# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,7
# Licence GPL v3

#should become methods of generic "distance"
#... could become the method to distinguish between Euclidean and great circle distances. 
#Methods should be defined for four column matrices, two column matrices (all combinations, resulting in a dist object) and SpatialPoints 

distanceEuclidean <- function (point1, point2) {
	if (length(point1) == 2) {
		x1 <- point1[1]
		y1 <- point1[2]
	} else {
		x1 <- point1[,1]
		y1 <- point1[,2]
	}
	if (length(point2) == 2) {
		x2 <- point2[1]
		y2 <- point2[2]
	} else {
		x2 <- point2[,1]
		y2 <- point2[,2]
	}
	distance <- sqrt((x1 - x2)^2 + (y1 - y2)^2)
	return(distance)
}

distanceGreatcircle <- function (point1, point2, r=6378137) {
# from degrees t o radians
	if ((length(point1) < 2) | (length(point2) < 2)) { stop('points should have at least 2 elements') } 

	point1 <- point1 * pi / 180
	point2 <- point2 * pi / 180	
	if (length(point1) == 2) {
		x1 <- point1[[1]]
		y1 <- point1[[2]]
	} else {
		x1 <- point1[[,1]]
		y1 <- point1[[,2]]
	}
	if (length(point2) == 2) {
		x2 <- point2[[1]]
		y2 <- point2[[2]]
	} else {
		x2 <- point2[[,1]]
		y2 <- point2[[,2]]
	}

#	cosd <- sin(y1) * sin(y2) + cos(y1) * cos(y2) * cos(x1-x2);
#	distance <- r * acos(cosd);
# supposedly more precise than above (http://en.wikipedia.org/wiki/Great_circle_distance):
	x <- sqrt((cos(y2) * sin(x1-x2))^2 + (cos(y1) * sin(y2) - sin(y1) * cos(y2) * cos(x1-x2))^2)
	y <- sin(y1) * sin(y2) + cos(y1) * cos(y2) * cos(x1-x2)
	distance <- r * atan2(x, y)
	return(distance)
}

# Author: Jacob van Etten jacobvanetten@yahoo.com
# International Rice Research Institute
# Date :  January 2009
# Version 1.0
# Licence GPL v3

setGeneric("distance", function(object, ...) standardGeneric("distance"))

setMethod("distance", signature(object = "RasterLayer"), def = function(object)
	{
		n <- ncell(object)
		fromCells <- which(!is.na(values(object)))
		toCells <- which(is.na(values(object)))
		accDist <- rep(0,times=n)
		if (isLatLon(object))
		{
			while(length(toCells)>0)
			{			
				adj <- adjacency(object,fromCells=fromCells,toCells=toCells,directions=8)
				coord <- cbind(xyFromCell(object,adj[,1]),xyFromCell(object,adj[,2]))
				distance <- apply(coord,1,function(x){distanceGreatcircle(x[1:2],x[3:4])})
				transitionValues <- accDist[adj[,1]] + distance
				transitionValues <- tapply(transitionValues,adj[,2],min)
				transitionValues <- transitionValues[transitionValues < Inf]
				fromCells <- as.integer(names(transitionValues))
				accDist[fromCells] <- transitionValues 
				toCells <- toCells[!(toCells %in% fromCells)]
			}
		}
		if (!(isLatLon(object)))
		{
			while(length(toCells)>0)
			{			
				adj1 <- adjacency(object,fromCells=fromCells,toCells=toCells,directions=4)
				adj2 <- adjacency(object,fromCells=fromCells,toCells=toCells,directions="Bishop")
				distance <- c(rep(1,length=length(adj1[,1])),rep(sqrt(2),length=length(adj1[,1])))
				adj <- rbind(adj1,adj2)
				transitionValues <- accDist[adj[,1]] + distance
				transitionValues <- tapply(transitionValues,adj[,2],min)
				transitionValues <- transitionValues[transitionValues < Inf]
				fromCells <- as.integer(names(transitionValues))
				accDist[fromCells] <- transitionValues 
				toCells <- toCells[!(toCells %in% fromCells)]
			}
		}
		result <- object
		result <- setValues(result, accDist)	
		return(result)
	}
)
