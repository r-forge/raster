\name{saveAs}

\alias{saveAs,RasterLayer,character-method}
\alias{saveAs,RasterBrick,character-method}
\alias{saveAs}

\title{Save a raster file to a different format}

\description{
Save a raster file to a file of another type (format). 
}

\usage{
saveAs(x, filename, ...)
}

\arguments{
  \item{x}{RasterLayer or RasterBrick object}
  \item{filename}{Character. Output filename}  
  \item{...}{additional arguments. See Details.}  
}


\details{
The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{filetype} \tab Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab output data type; see \code{\link[raster]{dataType}} \cr
  \tab \code{overwrite} \tab if TRUE, "filename" will be overwritten if it exists \cr
  \tab \code{progress} \tab Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""  \cr
 }
}


\value{
A RasterLayer or RasterBrick object. This primary use of this function is its side-effect of writing a file to disk.
}


\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{writeRaster}} }  

\examples{ 
r1 <- raster(system.file("external/test.ag", package="sp"))
r2 <- saveAs(r1, 'test.tif', filetype='GTiff')
}

\keyword{ spatial }
