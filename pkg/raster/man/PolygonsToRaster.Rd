\name{polygonsToRaster}

\alias{polygonsToRaster}

\title{Polygons to raster conversion}

\description{
Obsolete, see \link{rasterize}
}

\usage{
polygonsToRaster(p, raster, field=0, overlap='last', mask=FALSE, updateRaster=FALSE, updateValue="NA", getCover=FALSE, filename="", silent=FALSE, ...) 
}

\arguments{
  \item{p}{SpatialPolygons or a SpatialPolygonsDataFrame object }
  \item{raster}{Raster* object}
  \item{field}{The index (or colname) of the column in the SpatialPolygonsDataFrame to be transfered to the RasterLayer (see Details)}
  \item{overlap}{Character. Determines what values to assign to cells with overlapping polygons. Choose from \code{'first', 'last', 'sum', 'min'}, or \code{'max'}}
  \item{mask}{Logical. If \code{TRUE} the values of the input RasterLayer are 'masked' by the polygons. I.e. cells overlapping with the polygons retain their values, the other cells become NA }
  \item{updateRaster}{Logical. If \code{TRUE}, \code{raster} must be a RasterLayer. Its values are updated where the polygons overlap cells }
  \item{updateValue}{Character. Select areas to be updated with the polygon values (if \code{updateRaster == TRUE}) by the current cell values. Either \code{'all'}, \code{'NA'}, \code{'!NA'}, or \code{'zero'} } 
  \item{getCover}{Logical. If \code{TRUE}, the fraction of each grid cell that is covered by the polygons is returned (and the values of \code{field, overlap, mask}, and \code{updateRaster} are ignored. The fraction covered is estimated by dividing each cell into 100 subcells and determining presence/absence of the polygon in the center of each subcell}    
  \item{filename}{Character. Output filename }
  \item{silent}{Logical. If \code{TRUE}, feedback on the polygon count is suppressed}    
  \item{...}{Additional arguments. See Details.}    
}

\author{Robert J. Hijmans}

\seealso{ \code{\link{rasterize}} }


\keyword{ spatial }

