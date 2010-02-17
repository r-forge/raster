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

  
\section{III. Raster algebra}{
	\tabular{ll}{
    \code{\link{Arith-methods}} \tab Raster algebra. Arith generic function (\code{"+", "-", "*", "^", "\%\%", "\%/\%", "/"}) \cr
    \code{\link{Logic-methods}} \tab Raster algebra. Logic generic functions (\code{"!", "&", "|"}) \cr
    \code{\link{Compare-methods}} \tab Raster algebra. Compare generic functions (\code{"==", "!=", ">", "<", "<=", ">="}) \cr
	\code{\link{Math-methods}} \tab Raster algebra. Math generic functions (\code{"abs", "sqrt", "trunc", "log", "log10", "exp", "cos", "sin", "gamma", "round"}, and others)	\cr	
	}
}  
  
  
\section{IV. Functions for basic RasterLayer computation}{
  \tabular{ll}{
    \code{\link{calc}} \tab Computations on a single layer \cr
    \code{\link{overlay}} \tab Computations on mutliple layers \cr
    \code{\link{cover}} \tab First layer covers second layer except where the first layer is NA \cr
    \code{\link{mask}} \tab Values from first layer except where cells of the second layer are NA (these become NA)\cr
    \code{\link{reclass}} \tab Reclassify using a from-to-becomes matrix \cr
    \code{\link{subs}} \tab Substitute values \cr	
  }
}


\section{V. Functions for spatial contextual computation}{
  \tabular{ll}{
    \code{\link{distance}} \tab Distance to non-NA cells \cr
    \code{\link{gridDistance}} \tab Distance when forced to go through non-NA cells \cr
    \code{\link{focal}} \tab Focal (neighborhood) functions \cr
    \code{\link{focalFilter}} \tab Focal (neighborhood) filters \cr
    \code{\link{clump}} \tab Find clumps (patches) \cr
    \code{\link{adjacency}} \tab Identify cells that are adjacent to a set of cells on a raster \cr
    \code{\link{area}} \tab Compute area of cells (for longitude/latitude data) \cr
  }
}


\section{VI. Summarizing}{
  \tabular{ll}{
    \code{\link{zonal}} \tab Summarize a RatserLayer by zones in another RasterLayer \cr
    \code{\link{crosstab}} \tab Cross-tabulate two RasterLayer objects \cr
    \code{\link{freq}} \tab Frequency table of Raster cell values \cr
    \code{\link{count}} \tab Count the frequency of a single (range of) value(s) \cr
    \code{\link{cellStats}} \tab Summarize a RasterLayer using a function \cr
  }
}


\section{VII. Accessing values of Raster* object cells}{
  \tabular{ll}{
    \code{\link{getValues}} \tab Get all cell values (will fail with very large rasters and limited computer memory), or a row of values (safer) \cr
    \code{\link{getValuesBlock}} \tab Get values for a block (a rectangular area defined by start and end row and column \cr
    \code{\link{xyValues}} \tab Use points (optionally with a buffer) to extract cell values \cr
    \code{\link{cellValues}} \tab Use cell numbers to extract cell values \cr
    \code{\link{polygonValues}} \tab Use a SpatialPolygons* object to extract cell values \cr
  }
You can also use indexing with \code{[} for cell numbers, and \code{[[} for row / column number combinations \cr
}



\section{VIII. Plotting}{
  \tabular{ll}{
    \code{\link{plot}} \tab Plot a Raster* object (make a map)\cr
    \code{\link{hist}} \tab Histogram of Raster* object values \cr
    \code{\link{persp}} \tab Perspective plot of a RasterLayer \cr
    \code{\link{plot3D}} \tab 3-dimensional plot of a RasterLayer \cr
    \code{\link{contour}} \tab Contour plot of a RasterLayer \cr
    \code{\link{pairs}} \tab Pairs plot for layers in a RasterStack or RasterBrick \cr
  }
}


\section{IX. Getting and setting basic raster parameters}{

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


\section{X. Computing row, column, cell numbers and coordinates}{
  \tabular{ll}{
    \code{\link{xFromCol}} \tab x-coordinates from column numbers \cr
    \code{\link{yFromRow}} \tab y-coordinates from row numbers \cr
    \code{\link{xFromCell}} \tab x-coordinates from row numbers \cr
    \code{\link{yFromCell}} \tab y-coordinates from cell numbers \cr
    \code{\link{xyFromCell}} \tab x and y coordinates from cell numbers \cr
    \code{\link{colFromX}} \tab Column numbers from x-coordinates (or longitute) \cr
    \code{\link{rowFromY}} \tab Row numbers from y-coordinates (or latitute) \cr
    \code{\link{cellFromXY}} \tab Cell numbers from x and y coordinates \cr
    \code{\link{cellFromRowCol}} \tab Cell numbers from row and column numbers \cr
    \code{\link{cellsFromExtent}} \tab Cell numbers from extent object \cr
  }
}



\section{XI. Reading data}{
  \tabular{ll}{
    \code{\link{readAll}} \tab Read all values into memory \cr
    \code{\link{readRow}} \tab Read a row of data into memory \cr
    \code{\link{readBlock}} \tab Read a block of data into memory \cr
    \code{\link{values}} \tab Returns the values read with read* \cr
    \code{\link{dataContent}} \tab How much data (if any) is in memory? \cr
    \code{\link{dataIndices}} \tab Values for which cells are in memory? \cr	
    \code{\link{dataSource}} \tab What is the source of the values (disk or RAM)? \cr	
  }
  Rather then reading data & using values, it is avisble to use getValues* functions (see above)
}

  
\section{XII. Writing data}{
Basic writing
  \tabular{ll}{
    \code{\link{setValues}} \tab Put new values in a Raster* object \cr
    \code{\link{writeRaster}} \tab Write raster data to disk, either a row or all values \cr
    \code{\link{dataContent}} \tab How much data (if any) is in memory? \cr
  }
Advanced writing (more error prone, but faster)
  \tabular{ll}{
    \code{\link{writeStart}} \tab Open a file for writing \cr
    \code{\link{writeValues}} \tab Write some values  \cr
    \code{\link{writeStop}} \tab Close the file after writing \cr
	}
}


\section{Acknowledgements}{
We thank the following persons for suggestions to improve the package: Herry Herry, Paul Hiemstra, Rainer Krug, Alice Laborte, John Lewis, and Agustin Lobo.
}


\keyword{package}
\keyword{spatial}
