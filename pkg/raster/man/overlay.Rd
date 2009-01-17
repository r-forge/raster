\name{overlay}
\alias{Overlay}

\title{ Raster overlay calculation }
\description{
  Calculate values for a new RasterLayer object, based on two other rasters.  
  
}
\usage{
	Overlay(raster1, raster2, ..., fun=function(x,y){return(x+y)}, filename="", overwrite=FALSE) 
}

\arguments{
  \item{raster1}{ a RasterLayer object }
  \item{raster2}{ a RasterLayer object }
  \item{...}{ additional RasterLayer objects (up to 3) }
  \item{fun}{ the function to be appliepd. This should be a function that takes two numbers as an argument }
  \item{filename}{ filename for the output raster. A valid filename must be provided when the data of the input rasters are on disk }
  \item{overwrite}{logical. If \code{TRUE}, existing files will be overwritten}
}

\details{
  The default function of overlay sums the two rasters. Here are two other examples of functions that you can use:
  multiply: \code{fun=function(x,y){return(x*y)}}
  percentage: \code{fun=function(x,y){return(100 * x / y)}}
  
  some overlay functions have been implemented as generic functions (*, /, +, -)
  
}
\value{
  a new RasterLayer object (if a filename is provided, a file on disk) 
}
\author{ Robert J. Hijmans \email{r.hijmans@gmail.com} }

\examples{
	r <- newRaster()
	r1 <- init(r)
	r2 <- init(r)
	r3 <- Overlay(r1, r2)

# long version for multiplication
	r4 <- Overlay(r1, r2, fun=function(x,y){return(x*y)} )
# short function doing the same, if values can be loaded into ram memory
	r5 <- r1 * r2
}	

\keyword{ spatial }
