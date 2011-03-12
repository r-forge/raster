\name{hillShade}

\alias{hillShade1}
\alias{hillShade2}

\title{Hill-shade}

\description{
Compute hill shade from slope and aspect layers or from an elevation layer. Slope should be expressed as a fraction, and aspect layers should be in radians. Slope and aspect can be computed with \code{\link{slopeAspect}}. Elevation data should be in map units (typically meter for projected rasters), or in meters when the CRS is longitude/latitude. 

Hill-shade is often used as a backdrop on top of which another, semi-transparant, layer is drawn.
}

\usage{
hillShade1(alt, zenith, azimuth, filename='', ...)  
hillShade2(slope, aspect, zenith, azimuth, filename='', ...)  
}

\arguments{
  \item{alt}{RasterLayer object with elevation values in map units, or in meters when the crs is longitude/latitude}
  \item{slope}{RasterLayer object with slope values expressed as fraction }
  \item{aspect}{RasterLayer object with aspect values expressed as radians }
  \item{zenith}{ The height of the light source (sun) expressed in degrees}
  \item{azimuth}{ The direction of the light source (sun) expressed in degrees}
  \item{filename}{Character. Filename. optional}
  \item{...}{Standard additional arguments for writing RasterLayer files}
}

\seealso{ \code{\link{slopeAspect}} }

\author{Robert J. Hijmans & Andrew Bevan}

\keyword{spatial}

