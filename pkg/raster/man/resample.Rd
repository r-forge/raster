\name{resample}
\alias{resample}

\title{Resample}

\description{
Resample transfers values between non matching RasterLayers (in terms of origin and resolution). 

Avoid using this function if you can use functions like (dis)aggregate, crop and merge instead, because these functions are more accurate and much faster.
}

\usage{
resample(from, to, method="ngb", filename=NULL, filetype='raster', datatype='FLT4S', overwrite=FALSE, track=-1) 
}

\arguments{
  \item{from}{a RasterLayer object}
  \item{to}{a Raster* object}
  \item{method}{method used to compute values for the new RasterLayer}
  \item{filename}{character. output filename}  
  \item{overwrite}{Logical. If \code{TRUE}, the output file is overwritten if it already exists}
  \item{filetype}{output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeRaster}}}
  \item{datatype}{output data type; see \code{\link[raster]{setDatatype}}}
  \item{track}{vector of row numbers for which the function will report that they have been processed}  
}

\details{
valid values for \code{method} are \code{'ngb'} (nearest neighbor) and \code{'bilinear'} (bilinear interpolation).
}

\value{
  a new RasterLayer object, and, in some cases, the side-effect of the raster values written to disk 
}

\author{Robert J. Hijmans }


\seealso{ \code{\link[raster]{aggregate}}, \code{\link[raster]{crop}}, \code{\link[raster]{merge}} }


\examples{
r1 <- raster()
r1[] <- 1:ncell(r1)
r2 <- raster(xmn=-60.5, xmx=59.5, ymn=-30.5, ymx=29.5, nrow=15, ncol=30)
s <- resample(r1, r2)
}

\keyword{spatial}

