\name{bands}

\alias{band}
\alias{nbands}
  
\title{Bands}

\description{
A 'band' is term frequently used in remote sensing to refer to a variable (layer) in a multi-variable raster file or dataset. In remote sensing 
these bands could be stored in seperate files. Here a band refers to a single layer for a possibly multi-layer file. Most RasterLayer objects
will refer to files with a single band. \code{nbands} returns the number of bands of the file a RasterLayer points to (and 1 if it does not
point at any file), and \code{band} returns the specific band the RasterLayer refers to (1 if the RasterLayer points at single layer file or does not
point at any file). 
}

\usage{
band(object)
nbands(object)
}

\arguments{
  \item{object}{RasterLayer object}
}

\value{
a numeric value >= 1
}

\author{Robert J. Hijmans }
\examples{
r <- raster()
nbands(r)
band(r)
}

\keyword{spatial}
