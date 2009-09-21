\name{values}
\alias{values}
  
\title{Get raster values}

\description{
 values returns the cell values for a Raster* object that are currently in memory (typically after read* has been used).
}

\usage{
values(object, format='', names=FALSE)
}

\arguments{
 \item{object}{RasterLayer or RasterStack object}
 \item{format}{Character. specifiy the output format. Either '', 'vector', 'matrix' or 'dataframe'. If '' is used, a matrix is returned for a RasterStack, and a vector for a RasterLayer}
 \item{names}{Logical. If \code{TRUE}, and format='matrix', the returned matrix will have row and column names}
}
  

\details{
\code{values} are returned as a vector unless the option format='matrix' is used. In that case, a matrix is returned, optionally with the row and col numbers (names=TRUE). 
}

\value{
raster values
}

\author{Robert J. Hijmans}
\examples{
r <- raster(ncol=10, nrow=10)
r <- setValues(r, 1:ncell(r))
values(r)
r <- setValues(r, 1:ncol(r), row=5)
values(r)

}

\keyword{spatial}
\keyword{methods}
