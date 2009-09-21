\name{valuesRow}

\alias{valuesRow}
\alias{valuesRow,RasterLayer-method}
\alias{valuesRow,RasterStack-method}
\alias{valuesRow,RasterBrick-method}
  
\title{Get a row of raster values}

\description{
  valuesRow returns values for a row of values. It will take them from memory if available, else it will read them from disk. 
}

\usage{
valuesRow(x, rownr)
}

\arguments{
  \item{x}{RasterLayer or RasterStack object}
  \item{rownr}{The rownumber for which values are returned}
}
  

\details{
\code{values} are returned as a vector unless the option format='matrix' is used. In that case, a matrix is returned, optionally with the row and col numbers (names=TRUE). 
}

\value{
a row of raster values
}

\author{Robert J. Hijmans }
\examples{
r <- raster(ncol=10, nrow=10)
r <- setValues(r, 1:ncell(r))
valuesRow(r, 5)

}
\keyword{spatial}
\keyword{methods}
