\name{edge}

\alias{edge}
\alias{edge,RasterLayer-method}
  
\title{edge}

\description{
Detect edges. Edges are those cells who have more than one class in the 9 cells surrounding it, or, if \code{classes=FALSE}, cells with values and cells with \code{NA}. 
}

\usage{
edge(x, ...)
}

\arguments{
\item{x}{a Raster* object}
\item{...}{Additional arguments, see Details}
}

\details{
The following additional arguments can be passed:
\tabular{rll}{
  \tab \code{classes}  \tab Logical. If \code{TRUE} all different values are (after rounding) distinguished, as well as \code{NA}. If \code{FALSE}, the only comparison is between values and \code{NA} \cr
  \tab \code{type}  \tab Character. 'both' (default), 'inner', or 'outer' to distinguish cells with values versus cells with \code{NA}.  \cr

  \tab \code{filename}  \tab Character. Filename for the output RasterLayer \cr
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type; see \code{\link[raster]{dataType}} \cr
  \tab \code{overwrite} \tab Logical. If TRUE, "filename" will be overwritten if it exists \cr
  \tab \code{progress} \tab Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""  \cr
 }
}

\value{
a RasterLayer. Cell values are either 1 (and edge) or 0 (not an edge)
}

\note{
A call to edge with the default values is equivalent to call to \code{focal} with a neighborhood of 3 and \code{fun=function(x) length(unique(x))!=1}
}

\seealso{ \code{\link[raster]{focal}}, \code{\link[raster]{clump}} }


\author{Robert J. Hijmans}

\examples{
r <- raster(nrow=18, ncol=36)
r[] <- round(c(1:324, 324:1)/324)
r[r==0] <- NA
e <- edge(r)
}

\keyword{methods}
\keyword{spatial}
