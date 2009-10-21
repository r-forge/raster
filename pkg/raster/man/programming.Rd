\name{Function writing}

\alias{closeConnection}
\alias{openConnection}
\alias{canProcessInMemory}
\alias{pbSet} 
\alias{pbDo} 
\alias{pbClose} 

\title{ These are helper functions to use in new functions you develop}

\description{ 
low level functions for programmers 

\code{canProcessInMemory} is typically used within functions. In the raster package this function is used to determine if the amount of memory needed for the function is available. If there is not enough memory available, the function returns \code{FALSE}, and the function that called it will write the results to a temporary file. 
openConnection opens a file connection for reading, closeConnection removes it. 
}

\usage{
canProcessInMemory(raster, n=4)
closeConnection(raster)
openConnection(raster)
pbSet(nsteps, type, style=3)
pbDo(pb, step, label='row', value=step)
pbClose(pb, starttime)
}

\arguments{
  \item{raster}{a RasterLayer object}
  \item{n}{the number of copies of a RasterLayer object with values that a function needs to store in memory}  
  \item{nsteps}{Integer. Number of steps the progress bar will make from start to end (e.g. nrow(raster)) }
  \item{type}{Character. 'txt', 'tcltk', or 'windows'}
  \item{style}{style for text progress bar. See \code{\link[utils]{txtProgressBar}} }
  \item{pb}{a progress bar object created with setProgressBar }
  \item{step}{which step is this ( 1 <= step <= nsteps ) }
  \item{label}{for the non-text progress bars: a label}
  \item{value}{for the non-text progress bars: what value to paste after the label}
  \item{starttime}{Object of class "proc_time". If provided, the time to completion will be printed}
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
