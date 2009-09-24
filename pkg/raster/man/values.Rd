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
 \item{format}{Character. specifiy the output format. Either '', 'vector', 'matrix' or 'dataframe'. If '' is used, a matrix is returned for a RasterStack or RasterBrick, and a vector for a RasterLayer}
 \item{names}{Logical. If \code{TRUE}, and format='matrix' or 'dataframe', the returned matrix will have row and column names}
}
  

\details{
In most cases it is better to use \code{\link[raster]{getValues}} rather than \code{values}. \code{values} is typically used in functions after a read* function has been used and it will return all the values that happen to be in memory at that point, or fail if there are no values in memory. In contrast \code{getValues} will read values from disk if necessary. 

}

\value{
raster values
}

\seealso{
\code{\link[raster]{getValues}}, \code{\link[raster]{read}}
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
