\name{drawBbox}
\alias{drawBbox}

\title{ Draw Bounding Box }
\description{
  Click on two points of a plot (map) to obtain a BoundingBox object  
}
\usage{
drawBbox(show = TRUE, col = "red") 
}

\arguments{
  \item{show}{logical. If \code{TRUE}, the bounding box will be drawn on the map}  
  \item{col}{the color of the bounding box lines}    
 }
 
\value{
 a BoundingBox object
} 

\author{ Robert J. Hijmans }

\examples{

r1 <- raster(nrow=10, ncol=10)
r1[] <- runif(ncell(r1))
#plot(r1)
#bb <- drawBbox()
#mean(values(crop(r1, drawBbox())))



}


\keyword{ spatial }
