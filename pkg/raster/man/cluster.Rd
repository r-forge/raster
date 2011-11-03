\name{cluster}

\alias{beginCluster}
\alias{endCluster}
\alias{clusterR}

\title{Use a multi-core cluster}

\description{
\code{beginCluster} creates, and \code{endCluster} deletes a 'snow' cluster object. This object can be used for multi-core computing with a few 'raster' functions that support it. 

\code{beginCluster} determines the number of nodes (cores) that are available and uses all of them (unless the argument \code{n} is used). 

NOTE: beginCluster may fail when the package 'nws' is installed. You can fix that by removing the 'nws' package, or by setting the cluster type manually, e.g. beginCluster(type="SOCK")

endCluster closes the cluster and removes the object.

The use of the cluster by functions is automatic, but has only been implemented in a few functions (see Details).  

\code{clusterR} is a flexible interface for writing your own clusterized functions. This function only works with functions from the raster package that return Raster* objects with the same number of cells as the input raster object(s).
}

\usage{
beginCluster(n, type, nice)
endCluster()
clusterR(x, FUN, args=NULL, filename='', cl=NULL, m=2...)
}

\arguments{
\item{n}{Integer. The number of nodes to be used (optional)}
\item{type}{Character. The cluster type to be used (optional). E.g. "SOCK", see the snow package for details }
\item{nice}{Integer. To set the prioirty for the workers, between -20 and 20 (UNIX like platforms only)}
\item{x}{Raster* object}
\item{FUN}{function}
\item{args}{arguments for the function (excluding x, which should always be the first argument}
\item{filename}{character. Output filename (optional)}
\item{cl}{cluster object (do not use it if beginCluster() has been called}
\item{m}{tuning parameter to determine how many blocks should be used. The number is rounded and multiplied with the number of nodes.}
\item{...}{additional arguments for writing Raster objects to disk}
}

\details{
Functions that are 'cluster-enabled' include: \code{ \link{predict}, \link{projectRaster}, \link{resample}, \link{extract} (when using polygons)}.

If you want to write your own cluster-enabled functions see \code{\link{getCluster}, \link{returnCluster}}, and the vignette about writing functions
}

\value{
None. The side effect is to create or delete a cluster object.
}

\examples{

\dontrun{

#beginCluster()
beginCluster(type="SOCK")

r <- raster()
r[] = 1:ncell(r)

fun= function(x) x + 100
x = clusterR(r, fun, verbose=T)

fun2= function(x) calc(x, sqrt)
y = clusterR(r, fun2)

fun3= function(x) calc(x, range)
s = stack(r, r*2, r*3)
z = clusterR(s, fun3)

endCluster()
}
}

\author{Matteo Mattiuzzi and Robert J. Hijmans}

\keyword{ spatial }

