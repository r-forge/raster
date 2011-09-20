\name{bigRaster}

\docType{methods}

\alias{bigRaster}

\title{Create a BigRasterLayer object}

\description{
Methods to 
}


\arguments{
  \item{x}{RasterLayer}
  \item{...}{Additional arguments, see below }  
}

\usage{
bigRaster(x, ...)
}


\value{
BigRasterLayer
}

\author{Robert J. Hijmans}

\examples{

r <- raster(nrows=108, ncols=21, xmn=0, xmx=10)
r[] <- 1:ncell(r)

x <- bigRaster(r)
}

\keyword{methods}
\keyword{spatial}

