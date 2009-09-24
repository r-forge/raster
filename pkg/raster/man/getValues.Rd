\name{getValues}
\alias{getValues}
\alias{getValues,Raster,missing-method}
\alias{getValues,RasterStack,missing-method}
\alias{getValues,Raster,numeric-method}
\alias{getValues,RasterStack,numeric-method}
  
\title{Get raster values}

\description{
  getValues returns values for a row of values. It will take them from memory if available, else it will read them from disk. 
  This is the prefered function to get either all values, or a row of values from a RasterLayer
}

\usage{
getValues(x, row, ...)
}

\arguments{
 \item{x}{Raster* object}
 \item{row}{Numeric. Row number, should be between 1 and nrow(x), or missing in which case all values are returned}
 \item{...}{Additional arguments}
}
  

\details{
Additional arguments (only when all data are requeested):
 \code{format} Character. specifiy the output format. Either '', 'vector', 'matrix' or 'dataframe'. If '' is used, a matrix is returned for a RasterStack, and a vector for a RasterLayer
 \code{names} Logical. If \code{TRUE}, and format='matrix', the returned matrix will have row and column names
}

\value{
vector or matrix of raster values
}

\author{Robert J. Hijmans}
\examples{
r <- raster(system.file("external/test.ag", package="sp"))
getValues(r)
getValues(r, row=10)

}

\keyword{spatial}
\keyword{methods}
