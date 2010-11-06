\name{linesToRaster}

\alias{linesToRaster}

\title{Lines to raster}

\description{
Obsolete, see \link{rasterize}
}

\usage{
linesToRaster(lns, raster, field=0, overlap='last', mask=FALSE, updateRaster=FALSE, updateValue="NA", filename="", ...)
}

\arguments{
  \item{lns}{\code{SpatialLines} or a \code{SpatialLinesDataFrame} object (sp package), or an object inheriting from \code{SpatialLines}}
  \item{raster}{RasterLayer object}
  \item{field}{The index of the column in \code{lns} (if it is a \code{SpatialLinesDataFrame}) to be transfered to the \code{RasterLayer} (or the name of that field)}
  \item{overlap}{Character. Determines what values to assign to cells with overlapping lines. Choose from \code{'first', 'last', 'sum', 'min'}, or \code{'max'}}
  \item{mask}{Logical. If \code{TRUE} the values of the input RasterLayer are 'masked' by the lines. I.e. cells that are touched by the lines retain their values, the other cells become \code{NA} }
  \item{updateRaster}{Logical. If \code{TRUE} the values of the input RasterLayer are updated where the polygons overlap cells  }
  \item{updateValue}{Character. Select areas to be updated with the lines values (if \code{updateRaster == TRUE}) by the current cell values. Either 'all', 'NA', '!NA', or 'zero' } 
  \item{filename}{Character. output filename }
  \item{...}{Additional arguments}  
}

\author{Robert J. Hijmans }

\seealso{ \code{\link{rasterize}}}


\keyword{ spatial }
