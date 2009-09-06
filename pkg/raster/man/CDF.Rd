\name{CDF}
\docType{methods}

\alias{CDF}
\alias{CDF,RasterLayer,character-method}
\alias{CDF,RasterStack,character-method}

\title{Write a RasterLayer or RasterStack to a netCDF file}

\description{
Read the contents of a netCDF file into a RasterLayer or RasterStack object. Currently all the data are loaded into memory (
unlike with other formats), but this will likely change in the future. 
}

\usage{
CDF(x, y, ...)
}

\arguments{
\item{x}{Raster* object}
\item{y}{filename}
\item{...}{} 
}

\value{
Nothing. Function is used for the side effect of creating a netCDF file. 
}

\details{
  For this function to work, the RNetCDF package needs to have been installed.
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{rasterCDF} } }

\examples{
  fn <- 'test.nc'
  r <- raster(nrow=18, ncol=36)
  r[] <- 1:ncell(r)
  CDF(r, fn)
  r2 <- raster(fn)
}

\keyword{methods}
\keyword{spatial}

