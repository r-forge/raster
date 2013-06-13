\name{focalWeight}

\alias{focalWeight}

\title{Focal weights matrix}

\description{
Calculate focal ("moving window") weight matrix for use in the focal function.
}

\usage{
focalWeight(x, d, type=c('circle', 'Gauss', 'rectangle'))
}

\arguments{
\item{x}{Raster* object}
\item{d}{ xxx }
\item{type}{ yyy }
}

\value{
matrix
}


\examples{
r <- raster(ncols=36, nrows=18, xmn=0)
values(r) <- 1:ncell(r)
# Gaussian filter for square cells
gf <- focalWeight(r, c(5, 1.5), "Gauss")
rg <- focal(r, w=gf)
}


\keyword{spatial}

