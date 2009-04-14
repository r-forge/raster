\name{moveRasterFile}

\alias{moveRasterFile}
\alias{copyRasterFile}
\alias{removeRasterFile}
\alias{removeFile}

\title{move or copy a raster file}

\description{
Move, copy or remove 'raster' format files. removeRaster removes the files on disk that are associated with the RasterLayer object.
}

\usage{
moveRasterFile(raster, filename, overwrite=FALSE)
copyRasterFile(raster, filename, overwrite=FALSE)
removeRasterFile(filename)
removeFile(raster)
}

\arguments{
\item{raster}{RasterLayer object that is associated with a file on disk}
\item{filename}{Output filename}  
\item{overwrite}{Logical. If \code{TRUE} existing files are overwritten}
}

\value{
a RasterLayer
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{saveAs}} }  

\examples{ 
r <- raster(system.file("external/test.ag", package="sp"))
r <- saveAs(r, 'test.grd', filetype='raster', overwrite=TRUE)
rc <- copyRasterFile(r, 'test2.grd', overwrite=TRUE)
rm <- moveRasterFile(r, 'test3.grd', overwrite=TRUE)
}

\keyword{ spatial }
