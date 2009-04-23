\name{freq}

\alias{freq}

\title{Frequency table}

\description{
Frequncy table of the values of a RasterLayer.  
}

\usage{
freq(raster, digits=0)
}

\arguments{
  \item{raster}{A RasterLayer}
  \item{digits}{the number of digits for rounding the values}   
}

\value{
  A matrix
}


\seealso{ \code{\link[raster]{setMinMax} } }

\author{Robert J. Hijmans}

\examples{
r <- raster(nrow=18, ncol=36)
r[] <- runif(ncell(r))
r <- r * r * r * 10
freq(r)
}

\keyword{univar}
