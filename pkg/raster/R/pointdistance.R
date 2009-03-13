# Author: Robert J. Hijmans, r.hijmans@gmail.com and Jacob van Etten
# International Rice Research Institute
# Date :  June 2008
# Version 0,7
# Licence GPL v3



pointDistance <- function (point1, point2, type='Euclidean', ...) {
		
	if (!(type %in% c('Euclidean', 'GreatCircle'))) {
		stop('type should be Euclidean or GreatCircle')
	}	
	
	if (class(point1) == 'SpatialPoints' | class(point1) == 'SpatialPointsDataFrame') {
		point1 <- coordinates(point1)
	}
	if (class(point2) == 'SpatialPoints' | class(point2) == 'SpatialPointsDataFrame') {
		point2 <- coordinates(point2)
	}

	if ( (!is.vector(point1) & !is.matrix(point1)) |  (!is.vector(point2) & !is.matrix(point2)) ) {
		stop('points should be vectors of length 2, matrices with 2 columns, or a SpatialPoints* object')
	}

	if(is.vector(point1)){
		if (length(point1) != 2) {
			stop('point1. Wrong length for a vector, should be 2')
		} else {
			point1 <- matrix(point1, ncol=2) 
		}
	} else {
		if (length(point1[1,]) != 2) {
			stop('point1. The matrix should have 2 columns')
		}
	}

	if(is.vector(point2)){
		if (length(point2) != 2) {
			stop('point2. Wrong length for a vector, should be 2')
		} else {
			point2 <- matrix(point2, ncol=2) 
		}
	} else {
		if (length(point2[1,]) != 2) {
			stop('point2. The matrix should have 2 columns')
		}
	}
	
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


.greatCircleDist <- function(x1, y1, x2, y2, r = 6378137) {
	#	cosd <- sin(y1) * sin(y2) + cos(y1) * cos(y2) * cos(x1-x2);
	#	distance <- r * acos(cosd);
	#  the following is supposedly more precise than above (http://en.wikipedia.org/wiki/Great_circle_distance):
	x <- sqrt((cos(y2) * sin(x1-x2))^2 + (cos(y1) * sin(y2) - sin(y1) * cos(y2) * cos(x1-x2))^2)
	y <- sin(y1) * sin(y2) + cos(y1) * cos(y2) * cos(x1-x2)
	return ( r * atan2(x, y) )
}
