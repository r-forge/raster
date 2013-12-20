\name{combine}

\docType{methods}

\alias{combine}
\alias{combine,SpatialPolygons,SpatialPolygons-method}
\alias{combine,SpatialLines,SpatialLines-method}
\alias{combine,SpatialPoints,SpatialPoints-method}

\title{ 
combine parts of a Spatial* object
}

\description{
Combine (append) Spatial* objects into a single object. All objects must be of the same base class (SpatialPoints, SpatialLines, or SpatialPolygons)
}

\usage{
\S4method{combine}{SpatialPolygons,SpatialPolygons}(x, y, ..., keepnames=FALSE)

\S4method{combine}{SpatialLines,SpatialLines}(x, y, ..., keepnames=FALSE)

\S4method{combine}{SpatialPoints,SpatialPoints}(x, y, ..., keepnames=FALSE)
}


\arguments{
 \item{x}{Spatial* object}
 \item{y}{Spatial* object}
 \item{...}{Additional Spatial* objects}
 \item{keepnames}{Logical. If \code{TRUE} the row.names are kept (if unique)} 
}

\value{
Spatial* object
}


\keyword{methods}
\keyword{spatial}
