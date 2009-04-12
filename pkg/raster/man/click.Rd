\name{click}
\alias{click}

\title{ Click }

\description{
  Click on a plot (map) of a RasterLayer to get values of a Raster* object at a location; and optionally the coordinates for the location. 
  Multiple locations can be clicked on by specifying \code{n=}. 
}

\usage{
click(object, n=1, xy=FALSE, type = "n", ...)
}

\note{
  The plot only provides the coordinates (xy pair), the values are read from the Raster* object that is passed as an argument.
  Thus you can extract values for a Raster* object that is not plotted, as long as it shares the coordinate system (CRS) with the layer that is plotted.
}

\arguments{
  \item{object}{RasterLayer or RasterStack object}
  \item{n}{number of clicks on the map}
  \item{xy}{show xy coordinates}
  \item{type}{One of "n", "p", "l" or "o". If "p" or "o" the points are plotted; if "l" or "o" they are joined by lines. See ?locator}
  \item{...}{additional graphics parameters used if type != "n" for plotting the locations. See ?locator}   
 }
 
\value{
The value(s) of \code{object} at the point(s) clicked on; and the coordinates if \code{xy==TRUE}.
} 

\seealso{ \code{\link[raster]{drawBbox}} }

\author{ Robert J. Hijmans }

\examples{
 r <- raster(system.file("external/test.ag", package="sp"))
#plot(r)
#click(r)
#now click on the plot (map)
}

\keyword{ spatial }
