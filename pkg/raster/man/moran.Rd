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
  \item{filter}{Boolean. If \code{TRUE}, ngb should be a 'filter' (the local matrix of spatial weights). The value of the central cell should be (have a weight of) zero }
}


\value{
A single value (Moran's I) or a RasterLayer (Local Moran values)
}

\details{
The default setting uses a 3x3 neighborhoud to compute 'Queen's case' indices. You can use a filter to do other things, such as Rook's case, or different lags. 
}

\seealso{
The spdep package for additional and more general approaches for computing indices of spatial autocorrelation
}

\author{Robert J. Hijmans and Babak Naimi}

\references{
Moran, P.A.P., 1950. Notes on Continuous Stochastic Phenomena. Biometrika 37:17–23.
Anselin, L., 1995. Local Indicators of Spatial Association-LISA. Geographical Analysis 27:93–115
}


\examples{
r <- raster(nrows=10, ncols=10)
r[] <- 1:ncell(r)
Moran(r)
x1 <- MoranLocal(r)

# Rook's case
f <- matrix(c(0,1,0,1,0,1,0,1,0), nrow=3)
x2 <- MoranLocal(r, ngb=f, filter=TRUE)
}

\keyword{spatial}
