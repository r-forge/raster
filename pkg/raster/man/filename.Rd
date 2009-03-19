\name{filename}

\alias{setFilename}
\alias{filename<-}
\alias{filename}

\title{filename}

\description{
Get or set the filename of a Raster* object 
}

\usage{
filename(x)
filename(x) <- value
}

\arguments{
  \item{x}{A Raster* object }
  \item{value}{filename, including path, can be an empty string \code{""} }
}

\value{
a Raster* object
}


\author{ Robert J. Hijmans }

\examples{ 
r <- raster()
filename(r) <- "c:/test.grd"
filename(r)
}

\keyword{ spatial }

