\name{rasterToAccess}
\alias{rasterToAccess}

\title{ raster to Mdb }
\description{
  copy a RasterLayer to an Access database
}
\usage{
rasterToAccess(raster, filename)
}

\arguments{
  \item{raster}{ RasterLayer object}
  \item{filename}{ filename of Access database }
}
\details{
  If the Access database does not exist, it will be created. 
}
\value{
 a RasterLayer object
}


\author{Robert J. Hijmans}

\examples{
r <- raster(ncol=10, nrow=10)
r[] <- runif(100)
r2 <- rasterToAccess(r, 'AccessTest.mdb')
}

\keyword{ spatial }

