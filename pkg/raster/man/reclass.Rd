\name{reclass}

\alias{reclass}

\title{Reclassify values of a RasterLayer}
\description{
Reclassify values of a RasterLayer
}
\usage{
reclass(raster, rclmat, filename="", overwrite=FALSE, asInt = FALSE)
}

\arguments{
  \item{raster}{A RasterLayer object}
  \item{rclmat}{Matrix for reclassifcation. (see below)}
  \item{filename}{Output filename for a new raster; if NA the result is not written to a file but returned with the RasterLayer object, in the data slot}
  \item{overwrite}{Logical to indicate whether an existing output file should be overwritten}
  \item{asInt}{logical. If \code{TRUE}, output will be integer values}
}

\details{
\code{reclass} reclassifies groups of values to other values. E.g. All values between 1 and 10 become 1, and between 11 and 15 become 2. 
Reclassification is done with matrix "rclmat". This matrix must have 3 columns. The first two columns are "from" "to" of the input values, and the third column has the new value for that range.
	
If the input RasterLayer object has all values in memory (e.g. after readAll(raster)), the function will also return the new values in memory. If a filename is provided, the values will also be saved to that file. 
If the values are not in memory the new values will be written to file. 
}
\value{
A new RasterLayer object (in the R environment), and in some cases the side effect of a new file on disk.
}

\author{Robert J. Hijmans}

\examples{

r <- raster(ncols=36, nrows=18)
r[] <- runif(ncell(r)) 
# reclassify the values into three groups 
# all values between 0 and 0.25 become 1, etc.
m <- c(0, 0.25, 1,  0.25, 0.5, 2,  0.5, 1, 3)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc <- reclass(r, rclmat)
 
}
\keyword{spatial}
