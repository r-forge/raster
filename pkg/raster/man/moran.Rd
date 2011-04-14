\name{moran}

\alias{Moran}
\alias{MoranLocal}

\title{Moran index}

\description{
 Compute global or local Moran index (I) of spatial autocorrelation
}

\usage{
 Moran(x, ngb=3, filter=FALSE)
 MoranLocal(x, ngb=3, filter=FALSE)
}

\arguments{
  \item{x}{RasterLayer}
  \item{ngb}{The neighborhood (as defined in \code{\link{focal}}) or, if \code{filter=TRUE}, a "filter" (as defined in \code{\link{focalFilter}}) }
  \item{filter}{Boolean. If \code{TRUE}, ngb should be a 'filter' (matrix of spatial weights) }
}


\value{
A single value (global Moran) or a RasterLayer (local Moran)
}

\details{
The default setting uses a 3x3 neighborhoud to compute 'Queen's case' indices. You can use a filter to do other things, such as Rook's case, or different lags. 
}

\note{
The results with a neighborhood and a filter are currently not exactely the same; because if edge effects with focalFilter. This will hopefully be fixed soon.
See the spdep package for more flexibility
}

\author{Robert J. Hijmans and Babak Naimi}

\examples{
r <- raster(nrows=10, ncols=10)
r[] <- 1:ncell(r)
Moran(r)
x1 <- MoranLocal(r)

# Rook's case
f <- matrix(c(0,1,0,1,1,1,0,1,0), nrow=3)
x2 <- MoranLocal(r, ngb=f, filter=TRUE)
}

\keyword{spatial}
