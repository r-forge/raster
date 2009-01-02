\name{sp}
\alias{asRaster}
\alias{asSpgrid}
\alias{asBrick}

\title{ Functions to export a Raster to an sp object and to import a Raster from a sp object }

\description{  
	sp is ...
}

\usage{
asRaster(spgrid, getdata=TRUE, dataindex=1) 
asBrick(spgrid, getdata=TRUE) 
asSpgrid(raster, type='grid') 
}

\arguments{
  \item{raster} { a  Raster object }
  \item{spgrid} { a sp raster type object (SpatialPixel, SpatialPixelDataFrame , SpatialGrid, or SpatialGridDataFrame }
  \item{getdata} { logical. If \code{TRUE}, values are added to the Raster object }
  \item{dataindex} {integer values >= 1 indicating the column in the sp dataframe to take the values from}
  \item{type} {character. Indicating what type of sp object to return. Should be 'pixel' or 'grid'.}
}

\details{
 if \code{type} is 'pixel' a SpatialPixel* object is returned. If the raster object has data a SpatialPixelDataFrame will be returned.
 if \code{type} is 'grid' a SpatialGrid* object is returned. If the raster object has data a SpatialGridDataFrame will be returned.
}

\value{
	a Raster or a sp object
}
\author{ Robert J. Hijmans \email{r.hijmans@gmail.com} }

\examples{ 
	rs <- newRaster()
	rs <- setValues(rs, 1:ncells(rs))
	sp <- asSpgrid(rs)
	rs2 <- asRaster(sp) 
}

\keyword{ spatial }
