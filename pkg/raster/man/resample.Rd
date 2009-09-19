\name{resample}
\alias{resample}

\title{Resample}

\description{
Resample transfers values between non matching RasterLayers (in terms of origin and resolution). 

Avoid using this function if you can use functions like (dis)aggregate, crop and merge instead, because 
these functions are more accurate and much faster.
}

\usage{
resample(from, to, method="ngb", filename="", ...) 
}

\arguments{
  \item{from}{a RasterLayer object}
  \item{to}{a Raster* object}
  \item{method}{method used to compute values for the new RasterLayer}
  \item{filename}{character. output filename}  
  \item{...}{additional arguments. See Details.}    
 }

\details{
valid values for \code{method} are \code{'ngb'} (nearest neighbor) and \code{'bilinear'} (bilinear interpolation).


The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab overwrite \tab if TRUE, "filename" will be overwritten if it exists \cr
  \tab filetype \tab output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeRaster}} \cr
  \tab datatype \tab output data type; see \code{\link[raster]{dataType}} \cr
  \tab track \tab vector of row numbers for which the function will report that they have been processed  \cr
 }
}

\value{
  a RasterLayer object, and, in some cases, the side-effect of a file with the values of the new RasterLayer 
}

\author{Robert J. Hijmans }


\seealso{ \code{\link[raster]{aggregate}}, \code{\link[raster]{crop}}, \code{\link[raster]{merge}} }


\examples{
r <- raster(nrow=4, ncol=4)
r[] <- 1:ncell(r)
r2 <- raster(nrow=40, ncol=40)
s <- resample(r, r2, method='bilinear')
#plot(r)
#x11()
#plot(s)
}

\keyword{spatial}

