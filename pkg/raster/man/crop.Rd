\name{crop}

\alias{crop}

\title{Crop}

\description{
crop returns a geographic subset of the input RasterLayer as specified by a bounding box (or object that has a bounding box). 
Areas in the bounding box outside of the intput RasterLayer are ignored, and the Extent is aligned to the cells of the input RasterLayer. 
}

\usage{
crop(raster, extent, filename="", ...) 
}

\arguments{
  \item{raster}{A RasterLayer object}
  \item{extent}{Extent object, or any object that contain an Extent object (see below)}
  \item{filename}{Character. Output filename for a new raster}
  \item{...}{additional arguments. See Details.}  
}

\details{
 Objects that have a Extent object include RasterLayer, RasterStack. 
 They can also be extracted from objects descending from the Spatial class in the sp package. 
 You can check this with the \code{\link[raster]{extent}} function. New Extent objects can be created with \code{newExtent}.
 
 If no filename is specified, and the resulting RasterLayer is too large to hold in memory, it is saved to a temporary file.  

The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{overwrite} \tab if TRUE, "filename" will be overwritten if it exists \cr
  \tab \code{filetype} \tab output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab output data type; see \code{\link[raster]{dataType}} \cr
  \tab \code{progress} \tab Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""  \cr
 } 
}

\value{
A new RasterLayer object, and in some cases the side effect of a new file on disk.
}

\author{Robert J. Hijmans}

\seealso{ 
\code{\link[raster]{expand}}, \code{\link[raster]{merge}}
}

\examples{
r <- raster(nrow=45, ncol=90)
r[] <- 1:ncell(r)
bb <- newExtent(-160, 10, 30, 60)
rc <- crop(r, bb)	
}

\keyword{spatial}
