\name{setRowCol}

\alias{setRowCol}
\alias{ncol<-}
\alias{nrow<-}
\alias{setRes}
\alias{resolution<-}

\title{ Set properties of a RasterLayer }

\description{
Use these functions to set certain properties of a RasterLayer object. 
}

\usage{
setRowCol(object, nrows=nrow(object), ncols=ncol(object)) 
ncol(x) <- value
nrow(x) <- value
setRes(object, xres, yres=xres)
resolution(x) <- value
}

\arguments{
  \item{object}{A Raster* object }
  \item{x}{A Raster* object }
  \item{nrows}{ the new number of rows for the raster }
  \item{ncols}{ the new number of cols for the raster }  
  \item{xres}{ the new x resolution}
  \item{yres}{ the new y resolution}  
  \item{value}{depends on context. Can be row or column number or resolution }  
}

\details{
While you could set these values by accessing the slots of the Raster* object directly, 
you would do that at your own peril because the value of one raster property can affect values in other properties, 
and changing slot values may thus break the integrity of the objects. Using slots directly may also lead to code
to break in the future if underlying class definitions change. 
}

\value{
a Raster* object
}

\seealso{ \code{\link[raster]{extent}}, \code{\link[raster]{dimensions}}  }

\author{ Robert J. Hijmans }

\examples{ 
r <- raster()
r <- setRowCol(r, nrows=18, ncols=36) 
# equivalent to
nrow(r) <- 18
ncol(r) <- 36
r <- setRes(r, xres=1/120) 
# equivalent to
resolution(r) <- 1/120
# set yres diffrently
resolution(r) <- c(1/120, 1/60)

}

\keyword{ spatial }

