\name{Function writing}

\alias{closeConnection}
\alias{openConnection}
\alias{canProcessInMemory}

\title{ These are helper functions to use in your own functions }

\description{ 
low level functions for programmers 

\code{canProcessInMemory} is typically used within functions. In the raster package this function is used to determine wether values should be written to disk (e.g. in a temporary file) or not when there was no filename supplied to the function.

openConnection opens a file connection for reading, closeConnection removes it. 

}

\usage{
canProcessInMemory(raster, n=4)
closeConnection(raster)
openConnection(raster)
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
