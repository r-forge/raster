\name{disaggregate}
\alias{disaggregate}

\title{Disaggregate}

\description{
Disaggregate a RasterLayer to create a new RasterLayer with a higher resolution (smaller cells)
}

\usage{
disaggregate(raster, fact=2, filename="", ...) 
}

\arguments{
  \item{raster}{a RasterLayer object}
  \item{filename}{filename for the output RasterLayer}
  \item{fact}{degree of aggregation or disaggregation expressed as number of cells (horizontally and vertically). See details}
  \item{...}{additional arguments. See Details.}  
}

\details{
Fact can be a single integer or two integers c(x,y), in which case the first one is the horizontal disaggregation factor and y the vertical disaggreation factor. 
If a single integer values is supplied, cells are disaggregated with the same factor in x and y direction.

If no filename is specified, and the resulting RasterLayer is too large to hold in memory, it is saved to a temporary file.  

The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab overwrite \tab if TRUE, "filename" will be overwritten if it exists \cr
  \tab filetype \tab output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeRaster}} \cr
  \tab datatype \tab output data type; see \code{\link[raster]{dataType}} \cr
  \tab track \tab vector of row numbers for which the function will report that they have been processed  \cr
 }
}

\value{
A new RasterLayer object, and in some cases the side effect of a new file on disk.
}

\seealso{ \code{\link[raster]{aggregate}} }

\author{Robert J. Hijmans}

\examples{
r <- raster(ncols=10, nrows=10)
rd <- disaggregate(r, fact=c(10, 5))
ncol(rd)
nrow(rd)
}

\keyword{spatial}

