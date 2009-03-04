\name{aggregate}
\docType{methods}
\alias{aggregate}
\alias{aggregate,RasterLayer-method}
\title{Aggregate}

\description{
Aggregate a RasterLayer to create a new raster with a lower resolution (larger cells). Aggregation groups rectangular areas of RasterLayer cells to create a new RaterLayer with larger cells. 
A new value is computed for the resulting cells according to a user specified function (default value = \code{mean}). 
}

\section{Methods}{
\describe{
\code{aggregate(x, fact=2, fun=mean, expand=TRUE, na.rm=TRUE, filename=NULL, filetype='raster', datatype='FLT4S', overwrite=FALSE, track=-1) }
 
  \item{\code{x}}{a RasterLayer object}
  \item{\code{fun}}{function used to aggregate values}
  \item{\code{fact}}{integer. Aggregation factor expressed as number of cells in each direction (horizontally and vertically). Or two integers (horizontal and vertial aggregation fcator). See details}
  \item{\code{expand}}{logical. If \code{TRUE} the output RasterLayer will get larger if a division of the number of columns or rows with \code{factor} does not return an integer}
  \item{\code{na.rm}}{logical. If \code{TRUE}, remove NA cells from calculations}
  \item{\code{filename}}{character. output filename}
  \item{\code{overwrite}}{logical. If \code{TRUE}, "filename" will be overwritten if it exists}
  \item{\code{filetype}}{character. output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{rasterFormats}}}
  \item{\code{datatype}}{character. output data type; see \code{\link[raster]{setDatatype}}}
  \item{\code{track}}{vector of row numbers for which the function will report that they have been processed}   
}
}

\details{
 Aggregation will result in a RasterLayer with \code{fact*fact} fewer cells; sometimes adjusted according to the values of \code{expand}.
 For example, \code{fact=2} will result in a new RasterLayer with \code{2*2=4} times fewer cells. 
 If two numbers are supplied, e.g., \code{fact=c(2,3)}, the first will be used for aggregating in the horizontal direction, 
 and the second for aggregating in the vertical direction, and the new RasterLayer will have \code{2*3=6} times fewer cells.  
 
 If the input RasterLayer has 100 columns, and \code{fact=12}, the output RasterLayer will have either 8 columns (\code{expand=FALSE}) or 
 9 columns (\code{expand=TRUE}). The maximum x coordinate of the output RasterLayer will, of course, also be affected.
 
 Aggregation starts at the upper-left end of a raster.
 
 The function \code{fun} should take multiple numbers, and return a single number. For example \code{mean}, \code{min} or \code{max}. 
 
 If no filename is specified, and the resulting RasterLayer is too large to hold in memory, it is saved to a temporary file.  
 
}

\value{
A new RasterLayer object, and in some cases the side effect of a new file on disk.
}


\seealso{ \code{\link[raster]{disaggregate}} }

\author{Robert J. Hijmans and Jacob van Etten}

\examples{
r <- raster()
# a new aggregated raster, no values
ra <- aggregate(r, fact=10)
r <- setValues(r, runif(ncell(r)))
ra <- aggregate(r, fact=10, fun=max)
# a new aggregated raster, max of the values
}

\keyword{methods}
\keyword{spatial}

