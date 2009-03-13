\name{pointDistance}

\alias{distanceEuclidean}
\alias{distanceGreatcircle}

\title{distance between points}

\description{
  Calculate geographic distance between two points on a sphere (distanceGreatcircle) or on a plane (distanceEuclidean).
}

\usage{
distanceEuclidean(point1, point2)
distanceGreatcircle(point1, point2, r = 6378137)
}

\arguments{
  \item{point1}{  x and y coordinate of first (set of) point(s), either as c(x, y) or as matrix(ncol=2) in degrees for great-circle distance, in meters (or similar) for Euclidean }
  \item{point2}{  x and y coordinate of second (set of) second point(s). Like point1. If point1 and point2 are both matrices they should have the same number of rows }
  \item{r}{  radius of the world (modeled as a sphere), in meters }

}
\details{
  A sphere is an approximation for the earth (a spheroid) and distanceGreatcircle can thus be used with geographic 

  coordinates (latitude & longitude). There are probably better functions available in other packages that use a datum.
  You can use distanceEuclidean for coordinates of a map projection such as UTM (but note that in some projections distance is distorted). 

}

\value{
  A single value or a vector of values giving the distance in meters (great circle distance) or map-units (for instance, meters in the case of UTM)
}

\author{Robert J. Hijmans and Jacob van Etten }

\examples{
   a <- cbind(c(1,5,55,31),c(3,7,20))
   b <- cbind(c(4,2,8,65),c(50,-90,20))   

   distanceEuclidean(c(0, 0), c(1, 1))
   distanceGreatcircle(c(0, 0), c(1, 1))
   distanceGreatcircle(c(0, 0), a)
   distanceGreatcircle(a, b)
   
   #Make a distance matrix (dist object)
   dst <- matrix(nrow=4,ncol=4)
   for (i in 1:4){dst[,i] <- distanceGreatcircle(a[i,],a)}
   dst <- as.dist(dst)
   
   #Now the same, but avoiding the loop
   dst <- distanceGreatcircle(a[rep(1:4,times=4),],a[rep(1:4,each=4),])
   dst <- as.dist(matrix(dst,ncol=4))
}

\keyword{ spatial }
