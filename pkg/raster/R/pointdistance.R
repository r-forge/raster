# Author: Robert J. Hijmans  and Jacob van Etten
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3


.pointsToMatrix <- function(p) {
	if (class(p) == 'SpatialPoints' | class(p) == 'SpatialPointsDataFrame') {
		p <- coordinates(p)
	}
	if  (!is.vector(p) & !is.matrix(p))  {
		stop('points should be vectors of length 2, matrices with 2 columns, or a SpatialPoints* object')
	}
	if(is.vector(p)){
		if (length(p) != 2) {
			stop('Wrong length for a vector, should be 2')
		} else {
			p <- matrix(p, ncol=2) 
		}
	} else {
		if (length(p[1,]) != 2) {
			stop( 'A points matrix should have 2 columns')
		}
	}
	return(p)
}


pointDistance <- function (point1, point2, type='Euclidean', ...) {
	if (!(type %in% c('Euclidean', 'GreatCircle'))) {
		stop('type should be Euclidean or GreatCircle')
	}	

	point1 <- .pointsToMatrix(point1)
	point2 <- .pointsToMatrix(point2)
	
	if(length(point1[,1]) != length(point2[,1])) {
		if(length(point1[,1]) > 1 & length(point2[,1]) > 1) {
			stop('point1 and point2 do not have the same number of rows; and neither has only a single row')
		}
	}
	
	if (type == 'Euclidean') {
		return ( sqrt(( point1[,1] -  point2[,1])^2 + (point1[,2] - point2[,2])^2) )
	} else { 
		return(.greatCircleDist(point1[,1], point1[,2], point2[,1], point2[,2], ...) )
	}
}


.greatCircleDist <- function(x1, y1, x2, y2, r=6378137) {
	#	cosd <- sin(y1) * sin(y2) + cos(y1) * cos(y2) * cos(x1-x2);
	#	distance <- r * acos(cosd);
	#  the following is supposedly more precise than above (http://en.wikipedia.org/wiki/Great_circle_distance):
	x <- sqrt((cos(y2) * sin(x1-x2))^2 + (cos(y1) * sin(y2) - sin(y1) * cos(y2) * cos(x1-x2))^2)
	y <- sin(y1) * sin(y2) + cos(y1) * cos(y2) * cos(x1-x2)
	return ( r * atan2(x, y) )
}
