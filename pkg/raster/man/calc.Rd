\name{calculate}

\alias{calc}

\title{RasterLayer calculate}
\description{
  Calculate values for a new raster, from scratch or based on a single existing raster
}
\usage{
calc(raster, fun=sqrt, filename='', overwrite=FALSE, filetype='raster', datatype='FLT4S')
}

\arguments{
  \item{raster}{A RasterLayer object}
  \item{fun}{The function to be applied}
  \item{filename}{Output filename for a new raster; if NA the result is not written to a file but returned with the RasterLayer object, in the data slot}
  \item{overwrite}{Logical to indicate whether an existing output file should be overwritten}
  \item{filetype}{output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeRaster}}}
  \item{datatype}{output data type; see \code{\link[raster]{setDatatype}}}
}

\details{
Use \code{calc} to calculate values for a new raster using a function (\code{fun}) that takes a single number as an argument (and retruns a single number). E.g. to add, multiply or divide a number (cell value) by a constant.
	
If the input RasterLayer object has all values in memory (e.g. after readAll(raster)), the function will also return the new values in memory. If a filename is provided, the values will also be saved to that file. 
If the values are not in memory the new values will be written to file. 
}

\value{
A new RasterLayer object (in the R environment), and in some cases the side effect of a new file on disk.
}
\author{Robert J. Hijmans}

\examples{

r <- raster(ncols=36, nrows=18)
r[] <- 1:ncell(r)


# multiply values with 10
fun <- function(x) { x * 10 }
rc <- calc(r, fun)

# set values below 100 to NA. 
fun <- function(x) { x[x<100] <- NA; return(x) }
rc <- calc(r, fun)

# set NA values to -9999
fun <- function(x) { x[is.na(x)] <- -9999; return(x)} 
rc <- calc(rc, fun)
 
}

\keyword{spatial}
