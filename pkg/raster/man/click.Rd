\name{click}
\alias{click}
\alias{clickBbox}

\title{ Click on map }
\description{
  Click on a map to get values, coordinates or a bounding box
}
\usage{
click(object, n=1, xy=FALSE, type = "n", ...)
clickBbox(show = TRUE, col = "red") 
}

\arguments{
  \item{object}{RasterLayer or RasterStack object}
  \item{n}{number of clicks on the map}
  \item{xy}{show xy coordinates}
  \item{type}{One of "n", "p", "l" or "o". If "p" or "o" the points are plotted; if "l" or "o" they are joined by lines. See ?locator}
  \item{...}{additional graphics parameters used if type != "n" for plotting the locations. See ?locator}   
  \item{show}{logical. If \code{TRUE}, the bounding box will be drawn on the map}  
  \item{col}{the color of the bounding box lines}    
 }
 
\value{
click: the value of \code{object} at the point clicked on; and the coordinates if \code{xy==TRUE}.
clickBbox: a BoundingBox object
} 

\author{ Robert J. Hijmans }


\keyword{ spatial }
