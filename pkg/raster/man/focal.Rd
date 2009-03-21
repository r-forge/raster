\name{focal}

\alias{focal}
\alias{focalFilter}

\title{Focal}

\description{
Calculate values for the neighborhood of focal cells
}

\usage{
focal(raster, fun=mean, filename="", ngb=3, keepdata=TRUE, overwrite=FALSE, filetype='raster', datatype='FLT4S', track=-1) 
focalFilter(raster, filter, filename="", overwrite=FALSE, filetype='raster', datatype='FLT4S', track=-1) 
}

\arguments{
  \item{raster}{A RasterLayer object}
  \item{fun}{The function to be applied}
  \item{filename}{Output filename for a new raster}
  \item{ngb}{Neighborhood size. See Details.}
  \item{keepdata}{Logical. If \code{TRUE}, NA will be removed for neighborhood computations. The result will only be NA if all cells are NA}
  \item{overwrite}{Logical to indicate whether an existing output file should be overwritten}
  \item{filetype}{output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeRaster}}}
  \item{datatype}{output data type; see \code{\link[raster]{setDatatype}}}
  \item{track}{vector of row numbers for which the function will report that they have been processed}   
  \item{filter}{a matrix of weights. See Details}    
}

\details{
\code{focal} computes values based on those in the neighborhood of each cell. The values are computed with function 'fun', 
using the values of the cells in the neigborhood of a focal cell (including the focal cell).

A neighborhood is expressed in number of cells in a single direction or in two direction from the focal cell. I.e. \code{ngb=3} refers to 2 cells 
at each side of the focal cell. Queen's case, 8 cells in total. This is equivalent to \code{ngb=c(3,3)}. 
You can also specify a rectangular neighborhood, e.g. \code{ngb=c(3,5)}. Neighborhoods are normally odd numbers so that the focal
cell is centered, but even numbers are permitted in this function.

\code{focalFilter} does not use a function but a matrix of weights for the neigbhorhood of the focal cells. 
For example, \code{filter=matrix(1/9, nrow=3, ncol=3)} would be equivalent to \code{mean} with \code{ngb=3} in the \code{focal} function.

Gaussian filter:
\code{filter=matrix(c(1,2,3,2,1,2,3,4,3,2,3,4,5,4,3,2,3,4,3,2,1,2,3,2,1), nrow=5)/65}

Laplacian filter:
\code{filter=matrix(c(0,1,0,1,-4,1,0,1,0), nrow=3)}

Sobel filter:
\code{filter=matrix(c(1,2,1,0,0,0,-1,-2,-1) / 4, nrow=3)}

}

\value{
A new RasterLayer object (in the R environment), and in some cases the side effect of a new file on disk.
}

\author{Robert J. Hijmans}

\examples{

r <- raster(ncols=36, nrows=18)
r[] <- runif(ncell(r)) 
rf <- focal(r, fun=sum, ngb=3) 

gf=matrix(c(1,2,3,2,1,2,3,4,3,2,3,4,5,4,3,2,3,4,3,2,1,2,3,2,1), nrow=5)/65
rff <- focalFilter(r, filter = gf)
 
}
\keyword{spatial}

