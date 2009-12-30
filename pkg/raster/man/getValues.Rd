\name{getValues}
\alias{getValues}
\alias{getValues,RasterLayer,missing,missing-method}
\alias{getValues,RasterStack,missing,missing-method}
\alias{getValues,RasterBrick,missing,missing-method}
\alias{getValues,RasterLayer,numeric,missing-method}
\alias{getValues,RasterStack,numeric,missing-method}
\alias{getValues,RasterBrick,numeric,missing-method}
\alias{getValues,RasterLayer,numeric,numeric-method}
\alias{getValues,RasterStack,numeric,numeric-method}
\alias{getValues,RasterBrick,numeric,numeric-method}
  
\title{Get raster cell values}

\description{
  getValues returns values for a row of values. It will take them from memory if available, else it will read them from disk. 
  This is the prefered function to get either all values, or a subset of rows from a RasterLayer (it is safer then read* functions, which will fail when there is no file).
}

\usage{
getValues(x, row, nrows, ...)
}

\arguments{
 \item{x}{Raster* object}
 \item{row}{Numeric. Row number, should be between 1 and nrow(x), or missing in which case all values are returned}
 \item{nrows}{Numeric. Number of rows. Should be an integer number > 0, or missing}
 \item{...}{Additional arguments, see Details}
}
  

\details{
Additional arguments when x is a RasterLayer:
 \code{format} Character. specifiy the output format. Either '', 'vector', 'matrix' or 'dataframe'. If '' is used, a matrix is returned for a RasterStack or RasterBrick, and a vector for a RasterLayer

}

\value{
vector or matrix of raster values
}

\author{Robert J. Hijmans}
\examples{
r <- raster(system.file("external/test.grd", package="raster"))
getValues(r)
getValues(r, row=10)

}

\keyword{spatial}
\keyword{methods}
