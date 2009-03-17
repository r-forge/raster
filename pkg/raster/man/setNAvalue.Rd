\name{setNAvalue}

\alias{setNAvalue}

\title{set the NA value of a RasterLayer }

\description{
If you set the NA value of a RasterLayer, this value will be interpreted as NA when reading the values from a file. 
Values already in memory will not be affected. 

If the NA value is smaller than zero, all values smaller or equal to that number will be set to NA.
}

\usage{
setNAvalue(raster, value)
}

\arguments{
 \item{raster}{A \code{RasterLayer} object}
 \item{value}{the value to be interpreted as NA; set this before reading the values from the file. Integer values are matched exactely; for decimal values files any value <= the value will be interpreted as NA}  
}


\value{
a Raster* object
}


\author{ Robert J. Hijmans }

\examples{ 
r1 <- raster(system.file("pictures/Rlogo.jpg", package="rgdal"))
r2 <- setNAvalue(r1, 255)
#plot(r1)
#x11()
#plot(r2)
}

\keyword{ spatial }

