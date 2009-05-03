\name{tmpDir}

\alias{rasterTmpDir}
\alias{rasterTmpFile}
\alias{removeTmpFiles}
\alias{showTmpFiles}
\alias{setTmpDir}

\title{temporary files}

\description{
Functions in the raster package create temporary files if the values of an output RasterLayer cannot be stored in memory (ram). This can happen
when no filename is provided to a function and in functions where you cannot provide a filename (e.g. when using 'raster algebra').

The default location is returned by \code{rasterTmpDir}. It is the same as that of the R tempdir but you can change it (for the current session) to somewhere else with \code{setTmpDir}, for example if there is insufficient diskspace in that location. 

Temporary files are not automatically removed. You can use \code{showTmpFiles} to see what is there and \code{removeTmpFiles} to delete them. 
 \code{rasterTmpFile} returns a temporary filename. These can be useful when developing your own functions.

}

\usage{
rasterTmpDir
rasterTmpFile
showTmpFiles
removeTmpFiles
setTmpDir(d=NULL)
}

\arguments{
\item{d}{The new temporary files directory (for this session)}
}

\value{
\code{rasterTmpDir} returns the current directory for temporary files (for the Raster package)

\code{rasterTmpFile} returns a temporary file name (for use in functions)

\code{showTmpFiles} returns the names (.grd only) of the files in the temp directory

\code{removeTmpFiles} returns nothing

setTmpDir(d=NULL) sets a global option for the current sessions, but returns nothing
}

\details{
\code{removeRasterFile} can also take a filename as argument in stead of a RasterLayer. 
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[base]{tempdir}} }  


\keyword{ spatial }
