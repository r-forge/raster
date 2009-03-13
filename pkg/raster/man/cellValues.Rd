\name{cellValues}
\docType{methods}

\alias{cellValues}
\alias{cellValues,RasterLayer,vector-method}
\alias{cellValues,RasterStack,vector-method}

\title{ values for cells }

\description{
These methods return values of a RasterLayer or RasterStack for specified cells )
}

\usage{cellValues(x, cells)}

\arguments{
 \item{x}{RasterLayer or RasterStack object}
 \item{cells}{vector of cell numbers, cell numbers should be between 1 and ncells(x)}
}

\section{Methods}{
\describe{
 ---
}}


\value{
a vector of cell values for a RasterLayer or or a matrix of values for a RasterStack
}

\seealso{ \code{\link[raster]{xyValues}}, \code{\link[raster]{read}} }


\author{Robert J. Hijmans }


\examples{
#using a new default raster (1 degree global)
r <- raster()
r <- setValues(r, 1:ncell(r))
cellValues(r, c(1, 10, 100))

r <- raster(system.file("external/test.ag", package="sp"))
cells <- c(1, 1900, 5000)
cellValues(r, cells)

}


\keyword{methods}
\keyword{spatial}
