\name{dataProperties}

\alias{dataContent}
\alias{dataSource}

\title{Data properties}

\description{ 
These are helper functons for programmers and for debugging that provide information about the data values that are associated with the object. 

dataContent provides information regrading the values currently in memory (RAM). Possible values are: 'nodata', 'all', 'row', 'block'.
dataSource returns a value indicating whether the data source is a file on disk or whether it only exists in memory. Possible values are: 'ram' and 'disk'.
}

\usage{
dataContent(object)
dataSource(object)
}

\arguments{
  \item{object}{ Raster* object }
}

\value{
dataContent: 'nodata' or 'all'
dataSource: 'disk' or 'ram'
}

\author{Robert J. Hijmans}

\examples{
#using a new default raster (1 degree global)
rs <- raster()
dataContent(rs)
rs <- setValues(rs, 1:ncell(rs))
cellValues(rs, c(1, 10, 100))
dataSource(rs)
dataContent(rs)

rs <- raster(system.file("external/test.grd", package="raster"))
rs <- readAll(rs)
rs
dataSource(rs)
dataContent(rs)
}

\keyword{ spatial }
