\name{reclass}

\alias{reclass}

\title{Reclassify}

\description{
Reclassify values of a RasterLayer
}
\usage{
reclass(raster, rclmat, update=FALSE, filename="", overwrite=FALSE, filetype='raster', datatype='FLT4S', track=-1) 
}

\arguments{
  \item{raster}{A RasterLayer object}
  \item{rclmat}{Matrix (or vector) for reclassifcation. (see below)}
  \item{update}{Logical. If \code{TRUE} reclass updates previously reclassed values (see details)}
  \item{filename}{Output filename for a new raster; if NA the result is not written to a file but returned with the RasterLayer object, in the data slot}
  \item{overwrite}{Logical to indicate whether an existing output file should be overwritten}
  \item{filetype}{output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeRaster}}}
  \item{datatype}{output data type; see \code{\link[raster]{dataType}}}
  \item{track}{vector of row numbers for which the function will report that they have been processed}   
}

\details{
\code{reclass} reclassifies groups of values to other values. E.g. All values between 1 and 10 become 1, and between 11 and 15 become 2. 
Reclassification is done with matrix "rclmat". This matrix must have 3 columns. (You can also supply a vector that can be coecred into a n*3 matrix)
The first two columns are "from" "to" of the input values, and the third column has the new value for that range.

Reclassification is applied to  \code{from <= x <= to}. 
Reclassification is done in the order of the reclass table. Thus there are overlapping ranges, the last range applies.  

If \code{update=TRUE}, reclass can update values that were re-classified according to an earlier row in the reclass table. 
For example if row 1 has \code{1, 10, 15} and row 2 has \code{11, 20, 25}, all the values from 1 to 20 will be classified as 25. 
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
