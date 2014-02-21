\name{corr}

\docType{methods}

\alias{corr}
\alias{corr,RasterLayer,RasterLayer-method}
\alias{corr,RasterStackBrick,missing-method}


\title{Pearson correlation coefficient}

\description{
Pearson correlation coefficient between cell values of two RasterLayers or all layers in a RasterStack or RasterBrick
}

\usage{
\S4method{corr}{RasterLayer,RasterLayer}(x, y, ...)

\S4method{corr}{RasterStackBrick,missing}(x, y, ...)
}

\arguments{
  \item{x}{Raster* object}
  \item{y}{RasterLayer object if \code{x} is also a RasterLayer object; otherwise missing}
  \item{...}{additional arguments. None implemented}
}


\value{
numeric
}

\seealso{ \code{\link{cor}} }


\examples{
 b <- brick(system.file("external/rlogo.grd", package="raster"))
 corr(b)
}

\keyword{methods}
\keyword{spatial}

