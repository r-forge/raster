\name{focal}

\alias{focal}

\title{Focal}

\description{
Calculate values for the neighborhood of focal cells 
}

\usage{
focal(raster, fun=mean, filename="", ngb=3, keepdata=TRUE, ...) 
}

\arguments{
  \item{raster}{A RasterLayer object}
  \item{fun}{The function to be applied}
  \item{filename}{Output filename for a new raster}
  \item{ngb}{Neighborhood size. See Details.}
  \item{keepdata}{Logical. If \code{TRUE}, NA will be removed for neighborhood computations. The result will only be NA if all cells are NA}
  \item{...}{additional arguments. See Details.}  
}

\details{
\code{focal} computes values based on those in the neighborhood of each cell. The values are computed with function 'fun', 
using the values of the cells in the neigborhood of a focal cell (including the focal cell).

A neighborhood is expressed in number of cells in a single direction or in two direction from the focal cell. I.e. \code{ngb=3} refers to 2 cells 
at each side of the focal cell. Queen's case, 8 cells in total. This is equivalent to \code{ngb=c(3,3)}. 
You can also specify a rectangular neighborhood, e.g. \code{ngb=c(3,5)}. Neighborhoods are normally odd numbers so that the focal
cell is centered, but even numbers are permitted in this function.

The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{overwrite} \tab if TRUE, "filename" will be overwritten if it exists \cr
  \tab \code{filetype} \tab Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab output data type; see \code{\link[raster]{dataType}} \cr
  \tab \code{progress} \tab Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""  \cr
 }

}

\value{
A new RasterLayer object (in the R environment), and in some cases the side effect of a new file on disk.
}

\seealso{ \code{\link[raster]{focalFilter}} }


\author{Robert J. Hijmans}

\examples{

r <- raster(ncols=36, nrows=18)
r[] <- runif(ncell(r)) 
rf <- focal(r, fun=sum, ngb=3) 

}
\keyword{spatial}

