\name{overlay}
\alias{overlay,RasterLayer,RasterLayer-method}
\alias{Overlay}

\title{ Raster overlay calculation }
\description{
  Calculate values for a new RasterLayer object, based on two other rasters.  
}

\usage{
	Overlay(x, y, ..., fun, filename="", overwrite=FALSE, asInt=FALSE) 
}

\arguments{
  \item{x}{ a RasterLayer object }
  \item{y}{ a RasterLayer object }
  \item{...}{ additional RasterLayer objects (up to 6 in total, including x and y) }
  \item{fun}{ the function to be appliepd. This should be a function that takes two numbers as an argument }
  \item{filename}{ filename for the output raster. A valid filename must be provided when the data of the input rasters are on disk }
  \item{overwrite}{logical. If \code{TRUE}, existing files will be overwritten}
  \item{asInt}{logical. If \code{TRUE} the values will be rounded and stored as integer}
}

\details{
  The default function of overlay sums the two rasters. Here are two other examples of functions that you can use:
  multiply: \code{fun=function(x,y){return(x*y)}}
  percentage: \code{fun=function(x,y){return(100 * x / y)}}
  
  For smaller raster datasets, overlay functions can have been implemented through normal artimic functions (*, /, +, -) on RasterLayer objects (see examples)
  
}
\value{
  a new RasterLayer object (if a filename is provided, a file on disk) 
}
\author{ Robert J. Hijmans \email{r.hijmans@gmail.com} }

\examples{
	r <- newRaster()
	r1 <- init(r)
	r2 <- init(r)
	r3 <- Overlay(r1, r2, fun=function(x,y){return(x+y)})

# long version for multiplication
	r4 <- Overlay(r1, r2, fun=function(x,y){return(x*y)} )
# short function doing the same, if values can be loaded into ram memory
	r5 <- r1 * r2

# multiplication with more than two layers (make sure the number of RasterLayers matches the arguments of 'fun'
	r6 <- Overlay(r1, r2, r3, r4, fun=function(a,b,c,d){return(a*b+c*d)} )	
}	

\keyword{ spatial }
