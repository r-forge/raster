\name{select}

\docType{methods}

\alias{select}
\alias{select,Raster-method}
\alias{select,Spatial-method}

\title{
Geometric subsetting
}

\description{
Geometrically subset Raster* or Spatial* objects by drawing on a plot (map).
}


\usage{
\S4method{select}{Raster}(x, use='rec', ...)

\S4method{select}{Spatial}(x, use='rec', draw=TRUE, col='cyan', size=2, ...)
}

\arguments{
\item{x}{Raster*, SpatialPoints*, SpatialLines*, or SpatialPolygons*}
\item{use}{character: 'rec' or 'pol'. To use a rectangle or a polygon for selecting}
\item{draw}{logical. Add the selected features to the plot?}
\item{col}{color to use to draw the selected features (when \code{draw=TRUE)}}
\item{size}{integer > 0. Size to draw the selected features with (when \code{draw=TRUE)})}
\item{...}{additional arguments. None implemented}
}

\seealso{
\code{\link{click}, \link{crop}}
}


\value{
Raster* or Spatial* object
}


\examples{
\dontrun{
r <- raster(nrow=10, ncol=10)
r[] <- 1:ncell(r)
plot(r)
s <- select(r)
# now click on the map twice

# plot the selection on a new canvas:
x11()
plot(s)
}
}

\keyword{spatial}

