\name{sp}
\alias{as.raster}
\alias{as.spgrid}
\alias{as.brick}

\title{ Functions to export a Raster to an sp object and to import a Raster from a sp object }

\description{  
	sp is ...
}

\usage{
as.raster(spgrid, getdata=TRUE, dataindex=1) 
as.brick(spgrid, getdata=TRUE) 
as.spgrid(raster, type='grid') 
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
	rs <- raster.new()
	rs <- set.values(rs, 1:ncells(rs))
	sp <- as.spgrid(rs)
	rs2 <- as.raster(sp) 
}

\keyword{ spatial }
