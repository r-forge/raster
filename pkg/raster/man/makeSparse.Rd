\name{makeSparse}

\alias{makeSparse}

\title{ Make sparse}

\description{
Make a sparse RasterLayer object. 
}

\usage{
makeSparse(raster)
}

\arguments{
  \item{raster}{ A \code{RasterLayer} object }
}

\value{
a RasterLayer object
}
\author{ Robert J. Hijmans }

\examples{ 
	r <- raster()
	r[] <- runif(ncell(r))
	r[r > 0.001] <- NA
	rs <- makeSparse(r)
}

\keyword{ spatial }

