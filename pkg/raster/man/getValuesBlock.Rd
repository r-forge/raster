\name{getValuesBlock}

\alias{getValuesBlock}
\alias{getValuesBlock,RasterLayer,numeric-method}
\alias{getValuesBlock,RasterStack,numeric-method}
  
\title{Get raster cell values}

\description{
getValuesBlock returns values for a block (rectangular area) of values of a Raster* object. It will take them from memory if available, else it will read them from disk. 
}

\usage{
getValuesBlock(x, row, ...)
}

\arguments{
 \item{x}{Raster* object}
 \item{row}{Numeric. Row number, should be between 1 and nrow(x), or missing in which case all values are returned}
 \item{...}{Additional arguments, see Details}
}
  

\details{
many, t.b.d.
}

\value{
vector with raster values
}

\author{Robert J. Hijmans}
\examples{
r <- raster(system.file("external/test.grd", package="raster"))
getValuesBlock(r, row=2, nrows=3, col=2, ncols=15)
}

\keyword{spatial}
\keyword{methods}
