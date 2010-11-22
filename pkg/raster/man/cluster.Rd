\name{Cluster}

\alias{beginCluster}
\alias{endCluster}

\title{Use a cluster or multi-core CPU}

\description{
Create or delete a 'snow' cluster object. This object provides _experimental_ support for cluster or multi-core computing with a few 'raster' functions that support it. 

beginCluster creates the object 'raster_Cluster_raster_Cluster' in the global environment. It determines the number of nodes (cores) that are available and uses all of them (unless the argument \code{n} is used and it is a smaller number). 

endCluster closes the cluster and removes the object.
}

\usage{
beginCluster(n, type)
endCluster()
}

\arguments{
\item{n}{Integer. The number of nodes to be used (optional)}
\item{type}{Character. The cluster type to be used (optional)}
}

\details{
Functions that are 'cluster-enabled' are: \link{projectRaster}, \link{resample}, \link{distance}.
}

\value{
None. The side effect is to create or delete the cluster object.
}

\examples{
if (require(snow)) {

beginCluster()

r <- raster(ncol=36, nrow=18)
r[] <- NA
r[500] <- 1
dist <- distance(r) 

endCluster()

}
}

\author{Matteo Mattiuzzi and Robert J. Hijmans}

\keyword{ spatial }

