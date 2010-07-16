\name{rasterFromXYZ}
\docType{methods}

\alias{rasterFromXYZ}

\title{ Create a RasterLayer from x, y, z values }

\description{
Create a RasterLayer from x, y, z values 
}

\usage{
rasterFromXYZ(xyz, crs=NA, digits = 6)
}

\arguments{
\item{xyz}{Matrix or data.frame with three colomns: x and y coordinates and value z}
\item{crs}{Coordinate reference system}
\item{digits}{Precision for detecting wether points are on a regular grid (i.e., a low number of digits is a low precision)}
}

\value{
RasterLayer
}

\author{Robert J. Hijmans }

\examples{
r <- raster(nrow=10, ncol=10)
r[] <- runif(ncell(r))
r[r<0.5] <- NA
xyz <- rasterToPoints(r)
r2 <- rasterFromXYZ(xyz)
}

\keyword{methods}
\keyword{spatial}
