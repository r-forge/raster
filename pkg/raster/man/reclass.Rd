\name{reclass}

\alias{reclass}

\title{Reclassify}

\description{
Reclassify values of a RasterLayer
}
\usage{
reclass(raster, rclmat, filename="", overwrite=FALSE, filetype='raster', datatype='FLT4S', track=-1) 
}

\arguments{
  \item{raster}{A RasterLayer object}
  \item{rclmat}{Matrix (or vector) for reclassifcation. (see below)}
  \item{filename}{Output filename for a new raster; if NA the result is not written to a file but returned with the RasterLayer object, in the data slot}
  \item{overwrite}{Logical to indicate whether an existing output file should be overwritten}
  \item{filetype}{output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeRaster}}}
  \item{datatype}{output data type; see \code{\link[raster]{dataType}}}
  \item{track}{vector of row numbers for which the function will report that they have been processed}   
}

\details{
\code{reclass} reclassifies groups of values to other values. E.g. All values between 1 and 10 become 1, and between 11 and 15 become 2. 
Reclassification is done with matrix "rclmat". This matrix must have 3 columns. 
The first two columns are "from" "to" of the input values, and the third column has the new value for that range.
A reclass is applied to  \code{from <= x <= to}, in the order that they are in the reclass table.  

You can also supply a vector that can be coecred into a n*3 matrix
	
If the input RasterLayer object has all values in memory (e.g. after readAll(raster)), the function will also return the new values in memory. If a filename is provided, the values will also be saved to that file. 
If the values are not in memory the new values will be written to file. 
}

\value{
A new RasterLayer object, and in some cases the side effect of a new file on disk.
}

\author{Robert J. Hijmans}

\examples{

r <- raster(ncols=36, nrows=18)
r[] <- runif(ncell(r)) 
# reclassify the values into three groups 
# all values >= 0 and <= 0.25 become 1, etc.
m <- c(0, 0.25, 1,  0.25, 0.5, 2,  0.5, 1, 3)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc <- reclass(r, rclmat)

# equivalent to
m <- c(-Inf, 0.25, 1,  0.25, 0.5, 2,  0.5, Inf, 3)
rc <- reclass(r, m)
}

\keyword{spatial}
