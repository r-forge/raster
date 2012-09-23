\name{select}

\docType{methods}

\alias{select}
\alias{select,Raster-method}
\alias{select,SpatialPoints-method}
\alias{select,SpatialLines-method}
\alias{select,SpatialPolygons-method}

\title{
Select features from Spatial objects
}

\description{
Select spatial features from Raster*, SpatialPoints*, SpatialLines*, or SpatialPolygons* by drawing on a plot (map).
}


\usage{
\S4method{select}{Raster}(x, use='rec', ...)

\S4method{select}{SpatialPolygons}(x, use='rec', draw=TRUE, col='cyan', size=2, ...)

\S4method{select}{SpatialLines}(x, use='rec', draw=TRUE, col='cyan', size=2, ...)

\S4method{select}{SpatialPoints}(x, use='rec', draw=TRUE, col='cyan', size=2, ...)
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

\author{
Robert J. Hijmans
}

\keyword{spatial}

