\name{Function writing}

\alias{closeConnection}
\alias{openConnection}
\alias{canProcessInMemory}

\title{ These are helper functions to use in your own functions }

\description{ 
low level functions for programmers 

\code{canProcessInMemory} is typically used within functions. In the raster package this function is used to determine if the amount of memory needed for the function is available. If there is not enough memory available, the function returns \code{FALSE}, and the function that called it will write the results to a temporary file. 

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
canProcessInMemory: logical

closeHandle: RasterLayer object
}

\author{Robert J. Hijmans}

\examples{
r <- raster(nrow=100, ncol=100)
canProcessInMemory(r, 4)
r <- raster(nrow=100000, ncol=100000)
canProcessInMemory(r, 2)
}

\keyword{ spatial }
