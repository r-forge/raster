\name{advanced raster file writing}
\alias{closeHandle}

\title{ Create a new RasterLayer object }
\description{ low level functions for programmers 
}
\usage{
closeHandle(raster)
}

\arguments{
  \item{raster}{a RasterLayer object}
}
\details{
  closeHandle removes the handle to the file on disk that was established by rasterFromFile(). After the handle is closed, you can no longer read data from the file through the RasterLayer object. You probably do not need to do this; unless you get problems with files that cannot be overwritten (rm() does not immediately remove the handle; it seems). 
}

\value{
  RasterLayer object
}
\author{Robert J. Hijmans}

\examples{
}
\keyword{ spatial }
