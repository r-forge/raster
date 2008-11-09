\name{extract}
\alias{extract.cells}
\alias{extract.xy}


\title{ Extract values from a file into memory}
\description{
  Data for specified cells or c(x,y) coordinates are extracted from a Raster or RasterStack object 
}

\usage{
extract.cells(object, cells)
extract.xy(object, xy)

}

\arguments{
  \item{object}{ a Raster or RasterStack object }
  \item{cells}{ vector of cell numbers }
  \item{xy}{ matrix of x and y coordinates (i.e. 2 columns) }
}
\details{
}
\value{
  values for a set of points
}
\author{ Robert J. Hijmans \email{r.hijmans@gmail.com} }

\examples{

 rs <- raster.from.file(system.file("external/test.ag", package="sp"))

# read values for cells
  cells <- c(1, 1900, 5000)
  extract.cells(rs, cells)

# read values for (x, y) coordinates
  xy <- get.xy.from.cell(rs, cells)
  extract.xy(rs, xy)
  
}
  
\keyword{ spatial }
