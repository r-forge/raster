\name{zonal}

\alias{zonal}

\title{Zonal statistics}

\description{
Compute zonal statistics. I.e. the values of the cells of a RasterLayer based on the values of the "zones" RasterLayer. 
}

\usage{
zonal(raster, zones, stat='mean', keepdata=TRUE, track=-1) 
}

\arguments{
  \item{raster}{A RasterLayer object}
  \item{zones}{A RasterLayer object; with codes representing zones that are treated as integer values}
  \item{stat}{The function to be applied. Either as character: 'mean', 'min', 'max', 'sum'; or a function (see Details) }
  \item{keepdata}{Logical. If \code{TRUE}, NA will be removed for neighborhood computations. The result will only be NA if all cells are NA}
  \item{track}{vector of row numbers for which the function will report that they have been processed}   
}

\details{
If \code{stat} is a \code{function}, \code{zonal} will fail for very large RasterLayers
}

\value{
A data frame with a value for each zone
}

\author{Robert J. Hijmans}

\examples{
r <- raster(ncols=10, nrows=10)
r[] <- runif(ncell(r))
z <- r
z[] <- rep(1:5, each=20)
zonal(r, z, 'mean')
zonal(r, z, 'min')
zonal(r, z, 'sum')
# using a function, rather than a character value
zonal(r, z, sum)
}

\keyword{spatial}
