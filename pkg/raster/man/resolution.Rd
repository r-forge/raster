\name{resolution}

\alias{xres}
\alias{yres}
\alias{resolution}

\title{Resolution}

\description{
  Get resolution properties of a Raster* object
}

\usage{
xres(object)
yres(object)
resolution(object)
}

\arguments{
  \item{object}{Raster* object}
}


\value{
A single numeric value or two numeric values.
}

\seealso{ \code{\link[raster]{extent}}, \code{\link[raster]{dimensions}}  }


\author{Robert J. Hijmans }
\examples{
r <- raster(ncol=18, nrow=18)
xres(r)
yres(r)
resolution(r)
}

\keyword{spatial}
