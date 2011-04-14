\name{moran}

\alias{Moran}
\alias{MoranLocal}

\title{Moran index}

\description{
 Compute the global or local Moran index (I) of spatial autocorrelation
}

\usage{
 Moran(x) 
 MoranLocal(x)
}

\arguments{
  \item{x}{RasterLayer}
}

\details{
Only queen-case is supported
}

\value{
A single value (global Moran) or a RasterLayer (local Moran)
}

\author{Robert J. Hijmans}

\examples{
r <- raster(nrows=10, ncols=10)
r[] <- 1:ncell(r)
Moran(r)
MoranLocal(r)
}

\keyword{spatial}
