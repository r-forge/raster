\name{advanced writing}

\alias{closeHandle}
\alias{canProcessInMemory}

\title{ advanced writing }

\description{ 
low level functions for programmers 

canProcessInMemory is typically used within functions. It returns a logical value, indicating whether the values can be computed in memory.
In the raster package this function is used to determine wether values should be written to disk (in a temporary file) or not when there was
no filename supplied to the function.

closeHandle removes the handle to the file on disk that was established by rasterFromFile(). 
After the handle is closed, you can no longer read data from the file through the RasterLayer object. 
You probably do not need to do this; unless you get problems with files that cannot be overwritten (rm() 
does not immediately remove the handle; it seems). 
}

\usage{
canProcessInMemory(raster, n=4)
closeHandle(raster)
}

\arguments{
  \item{raster}{a RasterLayer object}
  \item{n}{the number of copies of a RasterLayer object with values that a function needs to store in memory}  
}


\value{
closeHandle: RasterLayer object
canProcessInMemory: logical
}

\author{Robert J. Hijmans}

\examples{
r <- raster(nrow=100, ncol=100)
canProcessInMemory(r, 4)
r <- raster(nrow=100000, ncol=100000)
canProcessInMemory(r, 2)
}

\keyword{ spatial }
