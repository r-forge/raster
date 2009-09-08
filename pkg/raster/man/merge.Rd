\name{merge}

\docType{methods}

\alias{merge}
\alias{merge,RasterLayer,RasterLayer-method}

\title{ 
Merge RasterLayers
}

\description{
Merge RasterLayers to form a new RasterLayer with a larger spatial extent
}

\usage{
merge(x, y, ...) 
}

\arguments{
  \item{x}{A RasterLayer object}
  \item{y}{A RasterLayer object}
  \item{...}{Additional RasterLayers and other arguments. See below, under Methods}  
}


\section{Methods}{
\describe{
A full call to the merge method is:

\code{merge(x, y, ..., tolerance=0.05, filename="", overwrite=FALSE, filetype='raster', track=-1 ) }

\item{x,y}{"RasterLayer"}{
  \item{\code{x}}{A RasterLayer object}
  \item{\code{y}}{A RasterLayer object}
  \item{\code{...}}{additional RasterLayer objects}
  \item{\code{tolerance}}{difference permissable (relative to the cell resolution) for objects to be 'equal'. See ?all.equal}
  \item{\code{filename}}{character. output filename}
  \item{\code{overwrite}}{logical. If \code{TRUE}, "filename" will be overwritten if it exists}
  \item{\code{filetype}}{character. output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeFormats}}}
  \item{\code{datatype}}{character. output data type; see \code{\link[raster]{dataType}}}
  \item{\code{track}}{vector of row numbers for which the function will report that they have been processed}   
}  
  
}} 

\details{
The RasterLayer objects must have the same origin and resolution. 
In areas where the RasterLayers overlap, the values of the RasterLayer that is first in the sequence of arguments will be retained. 
}

\value{
A new RasterLayer object, and in some cases, the side effect of a new file on disk.
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{crop}}, \code{\link[raster]{expand}}}

\examples{
r1 <- raster(xmx=-150, ymn=60, ncols=30, nrows=30)
r1[] <- 1:ncell(r1)
r2 <- raster(xmn=-100, xmx=-50, ymx=50, ymn=30)
res(r2) <- c(xres(r1), yres(r1))
r2[] <- 1:ncell(r2)
rm <- merge(r1, r2)
}

\keyword{methods}
\keyword{spatial}



