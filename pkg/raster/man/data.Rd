\name{properties}
\alias{dataIndices}
\alias{dataContent}
\alias{dataSource}

\title{ Get data properties }
\description{
}

\usage{
dataContent(object)
dataIndices(object)
dataSource(object)
}

\arguments{
  \item{object}{ Raster* object }
 }
  

\value{
	dataContent returns one of the following: nodata, all, row, block, sparse
	dataIndices returns the first and last cell number of the data in memory. Or all cell numbers for a sparse RasterLayer
	dataSource returns one of the following: ram, disk
	

}
\author{Robert J. Hijmans  \email{r.hijmans@gmail.com}}
\examples{
#using a new default raster (1 degree global)
rs <- raster.new()
dataContent(rs)
dataIndices(rs)
rs <- set.values(rs, 1:ncells(rs))
valuesCells(rs, c(1, 10, 100))
dataSource(rs)
dataContent(rs)
dataIndices(rs)

rs <- raster.from.file(system.file("external/test.ag", package="sp"))
rs <- readAll(rs)
rs
dataSource(rs)
dataContent(rs)
dataIndices(rs)

}
\keyword{ spatial }
