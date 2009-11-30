\name{getData}
\alias{getData}

\title{ getData }

\description{
  Get geographic data (first download if necessary)
}

\usage{
getData(name, download=TRUE, path='', ...)
}

\arguments{
  \item{name}{Data set name, currently supported are 'GADM' and 'SRTM'}
  \item{download}{Logical. If \code{TRUE} data will be downloaded if not locally avaialable}
  \item{path}{ path name to store the data. Default is the current working directory }
  \item{...}{additional paramenters, data set specific. See Details}   
}
 
\value{
A spatial object (Raster* or Spatial*)
} 

\details{  to be done  }

\author{ Robert J. Hijmans }


\keyword{ spatial }
