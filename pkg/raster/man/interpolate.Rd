\name{interpolate}

\docType{methods}

\alias{interpolate}
\alias{interpolate,Raster-method}

\title{Interpolate}

\description{
Make a RasterLayer with interpolated values based on a a fitted model object of classes such as 'gstat' or 'Krige'. You must have 'x' and 'y' as independent variables. If x and y are the only independent variables provide an (empty) RasterLayer for which you want predictions. If there are more spatial predictor variables provide these as a Raster* object. If you do not have x and y locations as predictors you should use \code{\link[raster]{predict}} instead.
}

\usage{
interpolate(object, ...) 
}

\arguments{
  \item{object}{a Raster* object}
  \item{...}{Additional arguments. See below, under Methods}  
}

\section{Methods}{
\describe{
 
\code{predict(object, model, filename='', ...)}
  
\tabular{rll}{  
  \tab  \code{object} \tab a RasterLayer object \cr
  \tab  \code{model}  \tab A fitted model\cr
  \tab  \code{filename}  \tab Output filename for a new raster; if NA the result is not written to a file but returned with the RasterLayer object, in the data slot \cr
  \tab  \code{...}  \tab Additional arguments, see below \cr
}

The following additional arguments can be passed, to replace default values
\tabular{rll}{
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type; see \code{\link[raster]{dataType}} \cr
  \tab \code{overwrite} \tab Logical. If TRUE, "filename" will be overwritten if it exists \cr
  \tab \code{progress} \tab Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""  \cr
 }
}}

\value{
a RasterLayer object
}

\details{
interpolate is a wrapper of raster::predict with argument xy=TRUE
}

\seealso{ \code{\link[raster]{predict}} }

\author{ Robert J. Hijmans}

\examples{
## Thin plate spline interpolation with x and y only
#r <- raster(system.file("external/test.ag", package="sp"))
#ra <- aggregate(r, 10)
#require(fields)
#tps <- Tps(as.data.frame(xyFromCell(ra, 1:ncell(ra))), values(ra) )
#p <- interpolate(r, tps)
#p[is.na(r)] <- NA
#plot(p)
## 


#library(gstat)
#r <- raster(system.file("external/test.ag", package="sp"))
#r <- raster(r)
#data(meuse)
#mg <- gstat(id = "zinc", formula = zinc ~ 1, locations = ~ x + y, data = meuse, nmax = 7, set = list(idp = .5))
#z <- interpolate(r, mg, xy=T)

}

\keyword{methods}
\keyword{spatial}
