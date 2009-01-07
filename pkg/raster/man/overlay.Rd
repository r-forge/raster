\name{Overlay}
\alias{Overlay}
\alias{cover}

\title{ Raster overlay calculation }
\description{
  Calculate values for a new RasterLayer object, based on two other rasters.  
}
\usage{
	Overlay(raster1, raster2, fun=function(x,y){return(x+y)}, filename="", overwrite=FALSE) 
	cover(raster1, raster2, filename="", overwrite=TRUE) 
}

\arguments{
  \item{raster1}{ a RasterLayer object }
  \item{raster2}{ a RasterLayer object }
  \item{fun}{ the function to be appliepd. This should be a function that takes two numbers as an argument }
  \item{filename}{ filename for the output raster. A valid filename must be provided when the data of the input rasters are on disk }
  \item{overwrite}{logical. If \code{TRUE}, existing files will be overwritten}
}

\details{
  The default function of Overlay sums the two rasters. Here are two other examples of functions that you can use:
  multiply: fun=function(x,y){return(x*y)}
  percentage: fun=function(x,y){return(100 * x / y)}
  
  some overlay functions have been implemented as generic functions (*, /, +, -)
  
  the \code{cover} function returns the values of the first raster, except where the first raster values are NA
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
	r4 <- setNA(r3, value=1)
	r5 <- cover(r4, r1)
# hist(r1) 
# hist(r3)	
# hist(r4)	
# hist(r5)	

# long version
	r6 <- Overlay(r1, r2, fun=function(x,y){return(x*y)} )
# short 'generic' function
	r7 <- r1 * r2
}	

\keyword{ spatial }
