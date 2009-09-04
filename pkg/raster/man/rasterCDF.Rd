\name{rasterCDF}

\alias{rasterCDF}
\alias{stackCDF}

\title{Read a netCDF file}

\description{Read the contents of a netCDF file into a RasterLayer or RasterStack object. Currently all the data are loaded into memory (unlike with other formats), but this will likely change in the future. 
}

\usage{
rasterCDF(filename, xvar='', yvar='', zvar='', time=1)
}

\arguments{
\item{filename}{filename}
\item{xvar}{x variable in the file (e.g. 'longitude')}
\item{yvar}{y variable in the file (e.g. 'latitude')}
\item{zvar}{z variable in the file (e.g. 'altitude', or 'precipitation)}
\item{time}{numer of time steps in the file (default is 1)} 
}

\value{
a Raster* object
}

\details{
  The function will try to guess values for the xvar, yvar, and zvar if they are not supplied.
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{raster},  \link[raster]{stack} } }

\examples{
}

\keyword{methods}
\keyword{spatial}

