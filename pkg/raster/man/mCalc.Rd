\name{mCalc}
\alias{mCalc}

\title{ multiple layers calculate }
\description{
  Calculate values for a new raster from the layers in a RasterStack
}

\usage{
mCalc(object, fun=sum, filename="", overwrite=FALSE, asInt=FALSE)
}

\arguments{
  \item{object}{ a RasterStack object }
  \item{fun}{ the function to be applied }
  \item{filename}{ output filename for a new raster; if NA the result is not written to a file but returned with the RasterLayer object, in the data slot }
  \item{overwrite}{ logical to indicate whether an existing output file should be overwritten }
  \item{asInt}{ logical. If \code{TRUE}, output will be integer values }
}

\details{
	\code{fun} should be a function that takes a one dimensional vector of value as input, and returns a single value.
	
	If the input raster has all values in memory (after readAll(raster)), the functions will also return the new data in memory. If there is a file name, the data will also be saved to that file. 
	If the values are not in memory the new values will be written to file. 
	}
\value{
	a new RasterLayer 
}
\author{ Robert J. Hijmans \email{r.hijmans@gmail.com} }

\examples{

 r <- newRaster(ncols=36, nrows=18)
 
# two ways to set all value to 1 
#1, with a function
 set1function <- function(x){rep(1, x)}
 r <- init(r, fun=set1function)
# 2, "manually" 
 r <- setValues(r, rep(1, ncell(r)))
 
# random values (see ?runif )
 r <- init(r, fun=runif)

# all values below 0.5 become NA
 r2 <- setNA(r, operator ="<", value=0.5)
# change the NAs to -9999
 r3 <- isNA(r2, -9999)  
 
# reclassify the values into three groups 
# all values between 0 and 0.25 become 1, etc.
 m <- c(0, 0.25, 1,  0.25, 0.5, 2,  0.5, 1, 3)
 rclmat <- matrix(m, ncol=3, byrow=TRUE)
 r4 <- reclass(r, rclmat)
 
#hist(r)
#hist(r2)
#hist(r3)
#hist(r4)
 
}
\keyword{ spatial }
