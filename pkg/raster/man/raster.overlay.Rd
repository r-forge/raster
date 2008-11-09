\name{overlay}
\alias{r.overlay}
\alias{r.cover}

\title{ Raster overlay calculation }
\description{
  Calculate values for a new raster object, based on two other rasters.  
}
\usage{
	r.overlay(raster1, raster2, fun=function(x,y){return(x+y)}, filename="", overwrite=TRUE) 
	r.cover(raster1, raster2, filename="", overwrite=TRUE) 
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{raster1}{ a raster object }
  \item{raster2}{ a raster object }
  \item{fun}{ the function to be appliepd. This should be a function that takes two numbers as an argument }
  \item{filename}{ filename for the output raster. A valid filename must be provided when the data of the input rasters are on disk }
  \item{overwrite}{}
}

\details{
  The default function of r.overlay sums the two rasters. Here are two other examples of functions that you can use:
  multiply: fun=function(x,y){return(x*y)}
  percentage: fun=function(x,y){return(100 * x / y)}
  
  r.cover returns the values of the first raster, except where the first raster values are NA
}
\value{
  a new raster object (if a filename is provided, a file on disk) 
}
\author{ Robert J. Hijmans \email{r.hijmans@gmail.com} }

\examples{
	r <- raster.new()
	r1 <- r.init(r)
	r2 <- r.init(r)
	r3 <- r.overlay(r1, r2)
	r4 <- r.setNA(r3, value=1)
	r5 <- r.cover(r4, r1)
# hist(r1) 
# hist(r3)	
# hist(r4)	
# hist(r5)	

}
\keyword{ spatial }
