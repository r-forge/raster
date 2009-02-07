\name{aggregate}
\docType{methods}
\alias{aggregate}
\alias{aggregate,RasterLayer-method}
\title{ Aggregate a RasterLayer }

\description{Aggregate a RasterLayer to create a new raster with a lower resolution. 
  \code{aggregate} is a generic function described in the package `stats'.}
}
\section{Methods}{
\describe{
\code{aggregate(x, fact = 2, fun = mean, expand = TRUE, rm.NA = TRUE, filename="", overwrite=FALSE, asInt = FALSE) }


\item{x}{ a RasterLayer object}
\item{filename}{output filename for a new raster}
  \item{fun}{function used to aggregate values. It should return one number. E.g. mean or max}
  \item{fact}{degree of aggregation or disaggregation expressed as number of cells (horizontally and vertically). See details}
  \item{expand}{if \code{expand == TRUE} grids will get larger if \code{factor} if the number of the division of the columns or rows with factor does not return an integer}
  \item{rm.NA}{if \code{rm.NA == TRUE}, remove NA cells from calculations}
  \item{asInt}{logical. If \code{TRUE} the values will be rounded and stored as integer}
  \item{overwrite}{if TRUE, "filename" will be overwritten if it exists}
}
}

\details{
 In aggregation \code{fact = 2} will result in a grid with 2 x 2 = 4 times fewer cells.
}

\value{
A new RasterLayer object (in the R environment), and in some cases the side effect of a new file on disk.
}
\author{Robert J. Hijmans}

\examples{
r <- raster()
r <- setValues(r, runif(ncell(r)))
ra <- aggregate(r, fact=10, fun=max)
}


\keyword{methods}
\keyword{spatial}






