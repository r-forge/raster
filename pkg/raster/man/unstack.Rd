\name{unstack}
\alias{unstack}
\alias{unstack,RasterStack-method}

\title{ Unstack }

\description{
Create or list of RasterLayer objects from a stack
}

\usage{
unstack(x, ...)
}

\arguments{
  \item{x}{ a RasterStack object }
  \item{...}{not used. further arguments passed to or from other methods}
}

\value{
  A list of RasterLayer objects
}
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{stack}}}

\examples{
  rasterfile <- system.file("external/test.ag", package="sp")
  st <- stack(rasterfile, rasterfile)
  rs <- unstack(st)
}

\keyword{ spatial }
\keyword{ methods }
