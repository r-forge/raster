\name{dimensions}

\alias{ncol}
\alias{nrow}
\alias{nrow,BasicRaster-method}
\alias{ncol,BasicRaster-method}
\alias{dim,BasicRaster-method}
\alias{ncell}
\alias{ncell,ANY-method}

\title{Dimensions}

\description{
  Get the number of rows, columns, or cells of a Raster* object
}

\usage{
ncol(x)
nrow(x)
ncell(x)
}

\arguments{
 \item{x}{a Raster* object}
}

\value{
 in most cases a single numeric value
}

\seealso{ \code{\link[raster]{extent}}, \code{\link[raster]{resolution}} }


\author{Robert J. Hijmans }
\examples{
r <- raster()
ncell(r)
ncol(r)
nrow(r)
}

\keyword{spatial}

