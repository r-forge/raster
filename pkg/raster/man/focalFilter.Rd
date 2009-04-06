\name{focalFilter}

\alias{focalFilter}

\title{Focal filter}

\description{
Calculate values for the neighborhood of focal cells using a matrix of weights
}

\usage{
focalFilter(raster, filter, fun=sum, filename="", overwrite=FALSE, filetype='raster', datatype='FLT4S', track=-1) 
}

\arguments{
  \item{raster}{A RasterLayer object}
  \item{filter}{a matrix of weights. See Details}    
  \item{fun}{a function to apply to the product of the matrix of weights and the values}
  \item{filename}{Output filename for a new raster}
  \item{overwrite}{Logical to indicate whether an existing output file should be overwritten}
  \item{filetype}{output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeRaster}}}
  \item{datatype}{output data type; see \code{\link[raster]{setDatatype}}}
  \item{track}{vector of row numbers for which the function will report that they have been processed}   
}

\details{
\code{focalFilter} uses a matrix of weights for the neigbhorhood of the focal cells, together with a function (normally sum). 
For example, \code{filter=matrix(1/9, nrow=3, ncol=3)} would be equivalent to \code{mean} with \code{ngb=3} in the \code{focal} function.

Gaussian filter:
\code{filter=matrix(c(1,2,3,2,1,2,3,4,3,2,3,4,5,4,3,2,3,4,3,2,1,2,3,2,1), nrow=5)/65}

Laplacian filter:
\code{filter=matrix(c(0,1,0,1,-4,1,0,1,0), nrow=3)}

Sobel filter:
\code{filter=matrix(c(1,2,1,0,0,0,-1,-2,-1) / 4, nrow=3)}

\code{focalFilter} can also be used to use non-rectangular areas. For example a combination of

\code{filter=matrix(c(0,0,0,0,1,1,0,1,1), nrow=3)} and \code{fun=max} returns the max value for the lower-rigth corner of a 3x3 matrix 
around a focal cell

}

\value{
A new RasterLayer object (in the R environment), and in some cases the side effect of a new file on disk.
}

\seealso{ \code{\link[raster]{focal}} }


\author{Robert J. Hijmans}

\examples{

r <- raster(ncols=36, nrows=18)
r[] <- runif(ncell(r)) 
gf=matrix(c(1,2,3,2,1,2,3,4,3,2,3,4,5,4,3,2,3,4,3,2,1,2,3,2,1), nrow=5)/65
rff <- focalFilter(r, filter = gf)
 
}
\keyword{spatial}

