\name{rasterize}

\docType{methods}

\alias{rasterize}
\alias{rasterize,matrix,Raster-method}
\alias{rasterize,SpatialPoints,Raster-method}
\alias{rasterize,SpatialLines,Raster-method}
\alias{rasterize,SpatialPolygons,Raster-method}
\alias{rasterize,Extent,Raster-method}

\title{Rasterize points, lines, or polygons}

\description{
Rasterize d

This function has replaced four other functions in this package (pointsToRaster, linesToRaster, polygonsToRaster)
}

\usage{
rasterize(x, y, ...) 
}

\arguments{
  \item{x}{points (a SpatialPoints* object or a two-column matrix), SpatialLines*, or SpatialPolygons*, or an Extent object }
  \item{y}{Raster* object}
  \item{...}{Additional arguments, see under Details}  
}

\details{

Additional arguments

\bold{\code{field}}. The index (or column name) of the variable in the Spatial*DataFrame to be transfered. If the Spatial* object has no DataFrame the \code{field} argument is ignored and the polygon index is used (i.e. numbers from 1 to the number of polygons). This is also 
done for Spatial*DataFrame objects when \code{field}==0. If  \code{field} < 0, all polygons get the value 1. In all cases you can also provide a vector with the same length as the number of spatial features, or a matrix where the number of rows matches the number of spatial features.
  
\bold{\code{fun}}. Determine what values to assign to cells overlapping with multiple spatial features. Either a functions such as mean, or one of the following character values: \code{'first', 'last', 'sum', 'min'}, or \code{'max'}

\code{mask}. Logical. If \code{TRUE} the values of the input Raster object are 'masked' by the spatial features of \code{x}. Cells that overlapwith the spatial features retain their values, the other cells become \code{NA}

\code{update}. Logical. If \code{TRUE}, the values of the Raster* object are updated for the cells that overlap the spatial features of \code{x}

\code{updateValue}. Character. Select areas to be updated with the polygon values (if \code{updateRaster == TRUE}) by the current cell values. Either \code{'all'}, \code{'NA'}, \code{'!NA'}, or \code{'zero'}

\code{filename}. Character. Output filename

\code{...} Additional arguments. 


\bold{= points =}

If \code{x} represents points

\code{background} =NA

\code{na.rm} =TRUE, 


\bold{= polygons =}

If \code{x} is a SpatialPolygons* or an Extent object

\code{getCover}{Logical. If \code{TRUE}, the fraction of each grid cell that is covered by the polygons is returned (and the values of \code{field, overlap, mask}, and \code{update} are ignored. The fraction covered is estimated by dividing each cell into 100 subcells and determining presence/absence of the polygon in the center of each subcell}    

\code{silent}{Logical. If \code{TRUE}, feedback on the polygon count is suppressed}    



\bold{= all cases =}

The following additional arguments can be passed in all cases when saving the output to a file
\tabular{rll}{
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{progress} \tab Character. "text", "window", or "" (the default, no progress bar)  \cr
 }
}


\value{
RasterLayer
}


\seealso{ \code{\link{values}} }


\author{Robert J. Hijmans}

\examples{

r <- raster(ncol=36, nrow=18)

###############################
# rasterize points
###############################

###############################
# rasterize lines
###############################

###############################
# rasterize polygons
###############################

###############################
# rasterize an extent
###############################


}

\keyword{methods}
\keyword{spatial}

