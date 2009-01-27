\name{adjacency}
\alias{adjacency}

\title{adjacency}
\description{
	Identify cells that are adjacent to a set of cells on a grid
}
\usage{
adjacency(raster, fromCells, toCells, directions) 
}

\arguments{
  \item{raster}{an object of the Raster family}
  \item{fromCells}{a vector of cell numbers for which adjacent cells should be calculated}
  \item{toCells}{a vector of cell numbers from which adjacent cells are selected}
  \item{directions}{in how many direction cells should be connected: 4, 8 or 16; or Bishop}
}

\details{
Cell numbers start with 1 in the upperleft corner and increase from left to right and from top to bottom.
Number of directions: 4 connects cells with one-cell rook moves, 8 with one-cell queen moves, and 16 with knight and one-cell queen moves.
Bishop connects cells with one-cell diagonal moves (special cases).
The function connects the outer meridians if the raster is in a geographic (latlon) projection and longitudes between -180 and 180 degrees are covered.
}

\value{
  a two column matrix with each row containing a pair of adjacent cells. 
}
\author{Jacob van Etten \email{jacobvanetten@yahoo.com}}

\examples{
	rs <- newRaster(nrows=10, ncols=10)
	adj <- adjacency(raster = rs, fromCells = c(1,30,55,72,100), toCells = c(1:ncell(rs)), directions=4) 
	
	#Visualize the outcome in a raster
	v <- vector(length=ncell(rs))
	v[adj[,2]] <- 2
	v[adj[,1]] <- 1
	rs <- setValues(rs, v)
	plot(rs)

	adjbis <- adjacency(raster = rs, fromCells = c(1,30,55,72,100), toCells = c(1:ncell(rs)), direction='Bishop') 
}

\keyword{spatial}
