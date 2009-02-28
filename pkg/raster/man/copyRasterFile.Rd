\name{moveRasterFile}

\alias{moveRasterFile}
\alias{copyRasterFile}

\title{move or copy a raster file}

\description{
  Move or copy a raster file to a different location (filename). 
}

\usage{
  moveRasterFile(raster, filename, overwrite=FALSE) 
  copyRasterFile(raster, filename, overwrite=FALSE) 
}

\arguments{
  \item{raster}{RasterLayer object that is associated with a file on disk}
  \item{filename}{output filename}  
  \item{overwrite}{Logical. If \code{TRUE} existing files are overwritten}
}

\details{
}

\value{
a RasterLayer
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{saveAs}} }  

\examples{ 
rs <- rasterFromFile(system.file("external/test.ag", package="sp"))
rs <- saveAs(rs, 'test.grd', filetype='raster', overwrite=TRUE)
rsc <- copyRasterFile(rs, 'test2.grd', overwrite=TRUE)
rsm <- moveRasterFile(rs, 'test3.grd', overwrite=TRUE)
 
}

\keyword{ spatial }
