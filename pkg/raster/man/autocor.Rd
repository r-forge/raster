\name{autocorrelation}

\alias{Geary}
\alias{Moran}
\alias{MoranLocal}

\title{Spatial autocorrelation}

\description{
Compute Moran's I or Geary's C measuers of global spatial autocorrelation in a RasterLayer, or compute the the local Moran index.
}

\usage{
Geary(x, w=3)
Moran(x, w=3)
MoranLocal(x, w=3)
}

\arguments{
  \item{x}{RasterLayer}
  \item{w}{Spatial weights. Either a single number or a vector of two numbers to define a neighborhood (as in \code{\link{focal}}) or, if \code{filter=TRUE}, a rectangular mattrix with uneven sides (a "filter" as in \code{\link{focalFilter}}) }
}


\value{
A single value (Moran's I) or a RasterLayer (Local Moran values)
}

\details{
The default setting uses a 3x3 neighborhoud to compute "Queen's case" indices. You can use a filter to do other things, such as Rook's case, or different lags. 
}

\seealso{
The spdep package for additional and more general approaches for computing indices of spatial autocorrelation
}

\author{Robert J. Hijmans and Babak Naimi}

\references{
Moran, P.A.P., 1950. Notes on continuous stochastic phenomena. Biometrika 37:17-23

Geary, R.C., 1954. The Contiguity Ratio and Statistical Mapping. The Incorporated Statistician 5: 115-145

Anselin, L., 1995. Local indicators of spatial association-LISA. Geographical Analysis 27:93-115

\url{http://en.wikipedia.org/wiki/Indicators_of_spatial_association}
}

\examples{
r <- raster(nrows=10, ncols=10)
r[] <- 1:ncell(r)
Moran(r)
Geary(r)

x1 <- MoranLocal(r)

# Rook's case
f <- matrix(c(0,1,0,1,0,1,0,1,0), nrow=3)
x2 <- MoranLocal(r, w=f)
}

\keyword{spatial}
