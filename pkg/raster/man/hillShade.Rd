\name{hillShade}

\alias{hillShade}

\title{Hillshade}

\description{
Compute hill shade from slope and aspect layers. Slope should be expressed as a tangent (fraction), and aspect layers should be in radians. Slope and aspect can be computed with \code{\link{slopeAspect}}. 

Hill-shade is often used as a backdrop on top of which another, semi-transparant, layer is drawn.
}

\usage{
hillShade(slope, aspect, zenith, azimuth, filename='', ...)  
}

\arguments{
  \item{slope}{RasterLayer object with slope values expressed as a fraction (tangent) }
  \item{aspect}{RasterLayer object with aspect values expressed as radians }
  \item{zenith}{ The height of the light source (sun) expressed in degrees}
  \item{azimuth}{ The direction of the light source (sun) expressed in degrees}
  \item{filename}{Character. Optional filename}
  \item{...}{Standard additional arguments for writing RasterLayer files}
}

\seealso{ \code{\link{slopeAspect}} }

\author{Andrew Bevan & Robert J. Hijmans}

\keyword{spatial}

