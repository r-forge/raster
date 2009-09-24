\name{clump}

\alias{clump}

\title{Detect clumps}

\description{
Detect clumps (patches) of connected cells. Each clump gets a unique integer ID.
}

\usage{
clump(raster, filename=NULL, ...) 
}

\arguments{
  \item{raster}{A RasterLayer object}
  \item{filename}{Output filename for a new raster}
  \item{...}{additional arguments. See Details.}  
}

\details{
The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{overwrite} \tab if TRUE, "filename" will be overwritten if it exists \cr
  \tab \code{filetype} \tab output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab output data type; see \code{\link[raster]{dataType}} \cr
  \tab \code{progress} \tab Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""  \cr
 }
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

