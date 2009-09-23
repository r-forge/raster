\name{makeSparse}

\alias{makeSparse}

\title{ Make sparse}

\description{
Make a sparse Raster* object. 
}

\usage{
makeSparse(object)
}

\arguments{
  \item{object}{ A \code{Raster*} object }
}

\value{
a sparse Raster* object
}

\details{
	Unlike other Raster* objects, sparse objects only store values for cells that are not NA (and an index). Sparse Raster* objects are currently 
	not well supported in the raster package and should not be used.
}

\author{ Robert J. Hijmans }

\examples{ 
	r <- raster()
	r[] <- runif(ncell(r))
	r[r > 0.001] <- NA
	rs <- makeSparse(r)
}

\keyword{ spatial }

