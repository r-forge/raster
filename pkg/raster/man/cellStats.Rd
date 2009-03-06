\name{cellStats}

\alias{cellStats}

\title{Statistics for the cells of a single RasterLayer}

\description{
  Compute statistics for the cells of a single RasterLayer. In this package, functions such as max, min, mean
  with one or more RasterLayer objects as argument, will return another RasterLayer. CellStats returns a single value, 
  computed from all the cell values of a single RasterLayer.
}

\usage{
cellStats(x, ..., na.rm=TRUE)
}

\arguments{
  \item{x}{A vector of numbers (typically integers for mode)}
  \item{...}{one or more applicable functions}
  \item{na.rm}{Remove (ignore) NA values}
}

\value{
  depends on the function(s) passed as ... argument. Typically a numeric value.
}

\details{
valid functions are those that take a numeric vector as argument, and return a single value. 
For example: \code{min}, \code{max}, \code{mean}

This function does not work for very large raster datasets (as all the data need to be loaded into memory).
}

\seealso{ \code{\link[raster]{setMinMax} } }

\author{Robert J. Hijmans}

\examples{
r <- raster(nrow=18, ncol=36)
r[] <- runif(ncell(r)) * 10
cellStats(r, min, max)
}

\keyword{univar}
