\name{Cell values}
\alias{cellValues}
\alias{xyValues}
\alias{xyValues,RasterLayer,SpatialPoints-method}
\alias{xyValues,RasterStack,SpatialPoints-method}
\alias{xyValues,RasterLayer,matrix-method}
\alias{xyValues,RasterStack,matrix-method}
\alias{cellValues,RasterLayer,vector-method}
\alias{cellValues,RasterStack,vector-method}
  
  
\title{Get raster properties}
\description{
  cellValues and xyValues return a vector of values for specified cells or xy coordiates
  minValue and maxValue return the extreme data values for a RasterLayer (or layer in a RasterStack)
}

\usage{
cellValues(object, cells)
xyValues(object, xyCoords)
}

\arguments{
  \item{object}{RasterLayer or RasterStack object}
  \item{cells}{vector of cell numbers}
  \item{xyCoords}{matrix of x and y coordinates (i.e. 2 columns)}
}
  

\note{
nothing  
}

\value{
raster cell value(s)
}

\author{Robert J. Hijmans }
\examples{
#using a new default raster (1 degree global)
r <- raster()
r <- setValues(r, 1:ncell(r))
cellValues(r, c(1, 10, 100))
xy <- cbind(1:20, 20:1)
xyValues(r, xy)

r <- rasterFromFile(system.file("external/test.ag", package="sp"))
# read values for cells
  cells <- c(1, 1900, 5000)
  cellValues(r, cells)

# read values for (x, y) coordinates
  xy <- xyFromCell(r, cells)
  xyValues(r, xy)

}
\keyword{spatial}
