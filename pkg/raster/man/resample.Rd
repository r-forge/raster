\name{resample}
\alias{resample}

\title{Resample}
\description{
	Resample
}
\usage{
resample(from, to, method="nngb", overwrite=FALSE)
}

\arguments{
  \item{from}{a RasterLayer object}
  \item{to}{a BasicRaster object (or any Raster* object}
  \item{method}{}
  \item{overwrite}{}
}

\details{
  resmaple
}

\value{
  a new RasterLayer object (in the R environment), with, in some cases, the side-effect of the raster values written to disk 
}
\author{Robert J. Hijmans \email{r.hijmans@gmail.com}}

\examples{
r1 <- newRaster()

}

\keyword{spatial}

