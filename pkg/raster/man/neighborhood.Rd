\name{neighborhood}

\alias{neighborhood}

\title{RasterLayer calculate}
\description{
Neighborhood calculations
}
\usage{
neighborhood(raster, fun=mean, filename="", ngb=3, keepdata=TRUE, overwrite=FALSE, asInt=FALSE) 
}

\arguments{
  \item{raster}{A RasterLayer object}
  \item{fun}{The function to be applied}
  \item{filename}{Output filename for a new raster; if NA the result is not written to a file but returned with the RasterLayer object, in the data slot}
  \item{ngb}{Neighborhood size. Expressed in number of cells in a single direction. I.e. 3 refers to 2 cells at each side of the focal cell. Queen's case, 8 cells in total}
  \item{keepdata}{Logical. If \code{TRUE}, NA will be removed for neighborhood computations. The result will only be NA if all cells are NA}
  \item{overwrite}{Logical to indicate whether an existing output file should be overwritten}
  \item{asInt}{logical. If \code{TRUE}, output will be integer values}
}

\details{
\code{neighborhood} computes a neighborhood value. I.e. a value computed with function 'fun', for all cells in the square neigborhood (of size ngb * ngb) around each cell 
If the input RasterLayer object has all values in memory (e.g. after readAll(raster)), the function will also return the new values in memory. If a filename is provided, the values will also be saved to that file. 
If the values are not in memory the new values will be written to file. 
	}
\value{
A new RasterLayer object (in the R environment), and in some cases the side effect of a new file on disk.
}
\author{Robert J. Hijmans}

\examples{

r <- raster(ncols=36, nrows=18)
r[] <- runif(ncell(r)) 
ngb <- neighborhood(r, fun=sum) 
 
}
\keyword{spatial}
