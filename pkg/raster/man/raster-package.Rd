\name{raster-package}
\alias{raster-package}

\docType{package}

\title{
Management, analysis and modeling of raster-type geographic data
}

\description{
This packages provides classes and functions to manipulate geographic (spatial) data in 'raster' form. 
Raster data divides the world in cells (rectangles; pixels) of equal size (in units of the projection). 
Such data are also referred to as 'grid' data.

The package should be particularly useful when using very large datasets that can not be loaded into the computer's memory. Functions will
work correctly, because they will read, process, and write blocks of data, without loading all values into memory. 

See the vignette for more information: \code{vignette('raster', 'raster')}
}

\details{
 The package implements classes for Raster data (see \link{Raster-class}) and supports
  \itemize{
    \item Creation of Raster* objects from scratch or from file
    \item Extremely large raster files
    \item Raster algebra and overlay functions
    \item Manipulation of raster extent, resolution and origin
    \item Distance, neighborhood (focal) and patch functions
    \item Polygon, line and point to raster conversion
    \item Summarizing raster values
    \item Model predictions
    \item Plotting (making maps)
    \item Computation of row, col and cell numbers to coordinates and vice versa
  }
}

\author{
Robert J. Hijmans and Jacob van Etten

Maintainer: Robert J. Hijmans \email{r.hijmans@gmail.com}
}


\section{I. Creating Raster* objects}{

Raster* objects can be created, from scracth or from raster files, with the following functions:

  \tabular{ll}{
    \code{\link{raster}}\tab To create a RasterLayer \cr
    \code{\link{stack}} \tab To create a RasterStack \cr
    \code{\link{brick}} \tab To create a RasterBrick \cr
  }
  
  
Basic parameters of existing Raster* objects can be obtained, and in most cases changed, with:  
  \tabular{ll}{
    \code{\link{ncol}}\tab The number of columns \cr
    \code{\link{nrow}} \tab The number of rows \cr
    \code{\link{ncell}} \tab The number of cells (can not be set directly, only via ncol or nrow) \cr
    \code{\link{res}} \tab The resolution (x and y) \cr
    \code{\link{xres}} \tab The x resolution (can be set with res) \cr
    \code{\link{yres}} \tab The y resolution (can be set with res)\cr
    \code{\link{xmin}} \tab The minimum x coordinate (or longitude) \cr
    \code{\link{xmax}} \tab The maximum x coordinate (or longitude) \cr
    \code{\link{ymin}} \tab The minimum y coordinate (or latitute) \cr
    \code{\link{ymax}} \tab The maximum y coordinate (or latitute) \cr
	\code{\link{extent}} \tab The extent (minimum and maximum x and y coordinates) \cr
    \code{\link{projection}} \tab The coordinate reference system (map projection) \cr
  }

If there are values associated with a RasterLayer object (either in memory or via a link to a file) these are lost when you change the number of columns or rows or the resolution. This is not the case when the extent is changed (as the number of columns and rows will not be affected). Simlarly, with \bold{projection} you can set the projection, but this does not transform the data (see projectRaster for that).   
}
  

\section{II. Chaging the spatial extent and/or resolution of RasterLayer objects}{

  \tabular{ll}{
    \code{\link{merge}} \tab Combine RasterLayer objects with different extents (but same origin and resolution) \cr
    \code{\link{crop}} \tab Select a geographic subset of a RasterLayer \cr
    \code{\link{expand}} \tab Enlarge a RasterLayer  \cr
    \code{\link{shrink}} \tab Shrink a RasterLayer by removing exterior rows and/or columns that only have NA values \cr
    \code{\link{aggregate}} \tab Combine cells to create larger cells \cr
    \code{\link{disaggregate}} \tab Subdivide cells \cr
    \code{\link{resample}} \tab Warp cell values to a RasterLayer with a different origin or resolution that can not easily be (dis)aggregated to \cr	
  }
}
  
  
\section{III. Computations with RasterLayer objects}{

  \tabular{ll}{
    \code{\link{calc}} \tab Computations on a single layer \cr
    \code{\link{overlay}} \tab Computations on mutliple layers \cr
    \code{\link{cover}} \tab First layer covers second layer except where the first layer is NA \cr
    \code{\link{mask}} \tab Values from first layer except where cells of the second layer are NA (these become NA)\cr
    \code{\link{reclass}} \tab Reclassify using a from-to-becomes matrix \cr
    \code{\link{subs}} \tab Substitute values \cr
  }
}


\section{IX. Plotting}{
  \tabular{ll}{
    \code{\link{plot}} \tab Plot a Raster* object (make a map)\cr
    \code{\link{hist}} \tab Histogram of Raster* object values \cr
    \code{\link{persp}} \tab Perspective plot of a RasterLayer \cr
    \code{\link{plot3D}} \tab 3-dimensional plot of a RasterLayer \cr
    \code{\link{contour}} \tab Contour plot of a RasterLayer \cr
    \code{\link{pairs}} \tab Pairs plot for layers in a RasterStack or RasterBrick \cr
  }
}


\section{X. Row, column, cell numbers and coordinates}{
  \tabular{ll}{
    \code{\link{xFromCol}} \tab x-coordinates from column numbers \cr
    \code{\link{yFromRow}} \tab y-coordinates from row numbers \cr
    \code{\link{xFromCell}} \tab x-coordinates from row numbers \cr
    \code{\link{yFromCell}} \tab y-coordinates from cell numbers \cr
    \code{\link{xyFromCell}} \tab x and y coordinates from cell numbers \cr
    \code{\link{colFromX}} \tab Column numbers from x-coordinates (or longitute) \cr
    \code{\link{rowFromY}} \tab Row numbers from y-coordinates (or latitute) \cr
    \code{\link{cellFromXY}} \tab Cell numbers from x and y coordinates \cr
    \code{\link{cellFromRowCol}} Cell numbers from row and column numbers \tab  \cr
    \code{\link{cellsFromExtent}} \tab cell numbers from extent object \cr
  }
}

  
  


\section{Acknowledgements}{
We thank the following persons for suggestions to improve the package: Herry Herry, Paul Hiemstra, Rainer Krug, Alice Laborte, John Lewis, and Agustin Lobo.
}


\keyword{package}
\keyword{spatial}
