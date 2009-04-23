\name{cellStats}

\alias{cellStats}

\title{Statistics for the cells of a single RasterLayer}

\description{
  Compute statistics for the cells of a single RasterLayer. In this package, functions such as max, min, mean
  with one or more RasterLayer objects as argument, will return another RasterLayer. CellStats returns a single value, 
  computed from all the cell values of a single RasterLayer.
}

\usage{
cellStats(raster, stat='mean', track=-1)
}

\arguments{
  \item{raster}{A RasterLayer}
  \item{stat}{The function to be applied. Either as character: 'mean', 'min', 'max', 'sum', or 'sd'; or a function (see Details) }
  \item{track}{vector of row numbers for which the function will report that they have been processed}   
}

\value{
  A numeric value.
}

\details{
If \code{stat} is a \code{function} rather than a character string, \code{zonal} will fail (gracefully) for very large RasterLayers.

\code{stat='sd'} returns slightly different values than \code{stat=sd}, because the former computes the standard deviation of the population
using this \code{sqrt((1/N) * sum(x^2) - mean(x)^2)} formula
}

\seealso{ \code{\link[raster]{setMinMax} } }

\author{Robert J. Hijmans}

\examples{
r <- raster(nrow=18, ncol=36)
r[] <- runif(ncell(r)) * 10
# works for large files
cellStats(r, 'mean')
# does not work for very large files
cellStats(r, mean)
}

\keyword{univar}
