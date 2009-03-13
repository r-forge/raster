# Author: Robert J. Hijmans, r.hijmans@gmail.com and Jacob van Etten
# International Rice Research Institute
# Date :  June 2008
# Version 0,7
# Licence GPL v3

distanceEuclidean <- function (point1, point2) {
	#some checks
	if ( (!is.vector(point1) & !is.matrix(point1)) |  (!is.vector(point2) & !is.matrix(point2)) ) {stop('points can only be supplied vectors of length 2 or matrices with 2 columns')}

	if(is.vector(point1)){
		if (length(point1) != 2) {stop('wrong length: point1 can only be a vector of length 2 or a matrix with 2 columns')}
	}
	if(is.vector(point2)){
		if (length(point2) != 2) {stop('wrong length: point2 can only be a vector of length 2 or a matrix with 2 columns')}
	}	
	if(is.matrix(point1)){
		if (length(point1[1,]) != 2) {stop('wrong length: point1 can only be a vector of length 2 or a matrix with 2 columns')}
	}
	if(is.matrix(point2)){
		if (length(point2[1,]) != 2) {stop('wrong length: point2 can only be a vector of length 2 or a matrix with 2 columns')}
	}
	if(is.matrix(point1) & is.matrix(point2)){
		if(length(point1[,1]) != length(point2[,1]))
			{stop('when point1 and point2 are both matrices they should have the same number of rows')}
	}
	
	distance <- sqrt((x1 - x2)^2 + (y1 - y2)^2)
	return(distance)
}

distanceGreatcircle <- function (point1, point2, r=6378137) {
	# some checks
	if ( (!is.vector(point1) & !is.matrix(point1)) |  (!is.vector(point2) & !is.matrix(point2)) ) {stop('points can only be supplied vectors of length 2 or matrices with 2 columns')}

	if(is.vector(point1)){
		if (length(point1) != 2) {stop('wrong length: point1 can only be a vector of length 2 or a matrix with 2 columns')}
	}
	if(is.vector(point2)){
		if (length(point2) != 2) {stop('wrong length: point2 can only be a vector of length 2 or a matrix with 2 columns')}
	}	
	if(is.matrix(point1)){
		if (length(point1[1,]) != 2) {stop('wrong length: point1 can only be a vector of length 2 or a matrix with 2 columns')}
	}
	if(is.matrix(point2)){
		if (length(point2[1,]) != 2) {stop('wrong length: point2 can only be a vector of length 2 or a matrix with 2 columns')}
	}
	if(is.matrix(point1) & is.matrix(point2)){
		if(length(point1[,1]) != length(point2[,1]))
			{stop('when point1 and point2 are both matrices they should have the same number of rows')}
	}
	
	# from degrees t o radians
	point1 <- point1 * pi / 180
	point2 <- point2 * pi / 180	

	#prepare x1,x2,y1,y2
	if(is.vector(point1)){
		x1 <- point1[1]
		y1 <- point1[2]
	}
	
	if(is.vector(point2)){
		x2 <- point2[1]
		y2 <- point2[2]
	}

	if(is.matrix(point1)){
		x1 <- point1[,1]
		y1 <- point1[,2]
	}

	if(is.matrix(point2)){
		x2 <- point2[,1]
		y2 <- point2[,2]
	}
	
	#	cosd <- sin(y1) * sin(y2) + cos(y1) * cos(y2) * cos(x1-x2);
	#	distance <- r * acos(cosd);
	#  the following is supposedly more precise than above (http://en.wikipedia.org/wiki/Great_circle_distance):
	x <- sqrt((cos(y2) * sin(x1-x2))^2 + (cos(y1) * sin(y2) - sin(y1) * cos(y2) * cos(x1-x2))^2)
	y <- sin(y1) * sin(y2) + cos(y1) * cos(y2) * cos(x1-x2)
	distance <- r * atan2(x, y)
	return(distance)
}
