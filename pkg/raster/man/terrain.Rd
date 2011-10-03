\name{terrain}

\alias{terrain}

\title{Terrain characteristics}

\description{
Compute slope, aspect and other terrain characteristics from elevation data. Elevation data should be in map units (typically meter) for projected (planar) raster data. They should be in meters when the coordinate reference system (CRS) is longitude/latitude. 

This function is under construction, as a replacement for \code{\link{slopeAspect}}
}

\usage{
terrain(x, filename='', out, unit='radians', neighbors=8, ...)  
}

\arguments{
  \item{x}{RasterLayer object with elevation values. Values should have the same unit as the map units, or in meters when the crs is longitude/latitude}
  \item{filename}{Character. Filename. optional}
  \item{out}{Character vector containing one or more of these options: slope, aspect, TPI, TRI, roughness (see Details)}
  \item{unit}{Character. 'degrees' or 'radians'. Only relevant for slope and aspect }
  \item{neighbors}{Integer. Indicating how many neighboring cells to use to compute slope for any cell. Either 8 (queen case) or 4 (rook case). Only used for slope and aspect, see Details}
  \item{...}{Standard additional arguments for writing Raster* objects to file}
}

\details{
TRI (Terrain Ruggedness Index) is the mean of the absolute differences between the value of a cell and the value of its 8 surrounding cells (Wilson et al. 2007).

TPI (Topographic Position Index) is the difference between the value of a cell and the mean value of its 8 surrounding cells (Wilson et al. 2007).

Roughness is the the the difference between the maximum and the minimum value of a cell and its 8 surrounding cells (Wilson et al, 2007).

These measures can also be computed with the \code{\link{focalFilter}} function:

f <- matrix(1, nrow=3, ncol=3)
TRI <- focalFilter(x, f, fun=function(x, ...) sum(abs(x[-5]-x[5]))/8 )
TPI <- focalFilter(x, f, fun=function(x, ...) x[5] - mean(x[-5]))
rough <- focalFilter(x, f, fun=function(x, ...) max(x) - min(x))


When \code{neighbors=4}, slope and aspect are computed according to Fleming and Hoffer (1979) and Ritter (1987). When \code{neigbors=8}, slope and aspect are computed according to Horn (1981). The Horn algorithm may be best for rough surfaces, and the Fleming and Hoffer algorithm may be better for smoother surfaces (Jones, 1997; Burrough and McDonnell, 1998). 

If slope = 0, aspect is set to 90 degrees (or 0.5*pi radians)
}

\seealso{ \code{\link{hillShade}} }


\references{
Burrough, P., and R.A. McDonnell, 1998. Principles of Geographical Information Systems. Oxford University Press.

Fleming, M.D. and Hoffer, R.M., 1979. Machine processing of landsat MSS data and DMA topographic data for forest cover type mapping. LARS Technical Report 062879. Laboratory for Applications of Remote Sensing, Purdue University, West Lafayette, Indiana.

Horn, B.K.P., 1981. Hill shading and the reflectance map. Proceedings of the IEEE 69(1):14-47

Jones, K.H., 1998. A comparison of algorithms used to compute hill slope as a property of the DEM. Computers & Geosciences 24(4): 315-323 

Ritter, P., 1987. A vector-based slope and aspect generation algorithm. Photogrammetric Engineering and Remote Sensing 53 (8):1109-1111

Wilson, M.F.J., O’Connell, B., Brown, C., Guinan, J.C., Grehan, A.J., 2007. Multiscale terrain analysis of multibeam bathymetry data for habitat mapping on the continental slope. Marine Geodesy, 30: 3-35.
}

\author{Robert J. Hijmans}

\keyword{spatial}
