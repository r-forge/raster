\name{clump}

\alias{clump}

\title{Detect clumps}

\description{
Detect clumps (patches) of connected cells. Each clump gets a unique integer ID.
}

\usage{
clump(raster, filename=NULL, overwrite=FALSE, filetype='raster', datatype='INT4S', track=-1) 
}

\arguments{
  \item{raster}{A RasterLayer object}
  \item{filename}{Output filename for a new raster}
  \item{overwrite}{Logical to indicate whether an existing output file should be overwritten}
  \item{filetype}{output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeRaster}}}
  \item{datatype}{output data type; see \code{\link[raster]{dataType}}}
  \item{track}{vector of row numbers for which the function will report that they have been processed}   
}

\details{
\code{clump} .
}

\value{
A new RasterLayer object (in the R environment), and in some cases the side effect of a new file on disk.
}

\author{Robert J. Hijmans}

\examples{
r <- raster(ncols=50, nrows=50)
r[] <- round(runif(ncell(r))*0.75 )
rc <- clump(r) 
#freq(rc)
}
\keyword{spatial}

