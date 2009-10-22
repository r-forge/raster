\name{filename}

\alias{filename}
\alias{filename<-}

\title{filename}

\description{
Get the filename of a Raster* object. You can set the filename of a new RasterLayer with \code{\link[raster]{raster}} with a RasterLayer object as first argument (and similarly for a RasterBrick. 
}

\usage{
filename(x)
filename(x) <- value
}

\arguments{
  \item{x}{A Raster* object }
  \item{value}{A filename (only for RasterStack objects }
  
}

\value{
a Raster* object
}


\author{ Robert J. Hijmans }

\examples{ 
r <- raster( system.file("external/test.ag", package="sp") )
filename(r)
}

\keyword{ spatial }

