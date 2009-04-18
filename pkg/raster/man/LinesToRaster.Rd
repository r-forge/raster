\name{linesToRaster}

\alias{linesToRaster}

\title{Lines to raster}

\description{
  Lines to raster conversion. Lines are raserized.
}

\usage{
linesToRaster(spLines, raster, field=0, filename="", overwrite=FALSE, updateRaster=FALSE, updateValue="NA", filetype='raster', datatype='FLT4S', track=-1) 
}

\arguments{
  \item{spLines}{ a SpatialLines or a SpatialLinesDataFrame object (sp package)}
  \item{raster}{ a RasterLayer object}
  \item{field}{ The index of the column in the SpatialLinesDataFrame to be transfered to the RasterLayer }
  \item{filename}{ output filename }
  \item{overwrite}{ logical. if \code{TRUE} ouput file will be overwritten if it exists }
  \item{updateRaster}{logical. If \code{TRUE} the values of the input RasterLayer are updated where the polygons overlap cells  }
  \item{updateValue}{character. Select cells to be updated (if \code{updateRaster == TRUE}) by their current values. Either 'all', 'NA', '!NA', or 'zero' } 
  \item{filetype}{output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeRaster}}}
  \item{datatype}{output data type; see \code{\link[raster]{dataType}}}
  \item{track}{vector of row numbers for which the function will report that they have been processed}    
}

\details{
For SpatialLines, the 'field' argument is ignored. The line index is used as attribute,
}

\author{Robert J. Hijmans }

\seealso{ \code{\link[raster]{polygonsToRaster}}, \code{\link[raster]{pointsToRaster}}}

\examples{
cds1 <- rbind(c(-50,0), c(0,60), c(40,5), c(15,-45), c(-10,-25))
cds2 <- rbind(c(80,20), c(140,60), c(160,0), c(140,-55))
cds3 <- rbind(c(-180,-20), c(-140,-60), c(-60,-20), c(-140,55))

lines <- SpatialLines(list(Lines(list(Line(cds1)), "1"), Lines(list(Line(cds2)), "2"), Lines(list(Line(cds3)), "3") ))

r <- raster(ncols=90, nrows=45)
r <- linesToRaster(lines, r)
#plot(r)
#plot(lines, add=TRUE)
}

\keyword{ spatial }
