# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,7
# Licence GPL v3

distance.euclidean <- function (point1, point2) {
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

distance.greatcircle <- function (point1, point2, r=6378137) {
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

