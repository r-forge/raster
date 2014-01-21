\name{metadata}

\alias{metadata}
\alias{metadata<-}

\title{ Metadata }

\description{
Get or set a metadata to a Raster object 
}

\usage{
metadata(x) 
metadata(x) <- value
}

\arguments{
  \item{x}{Raster* object }
  \item{value}{list with named elements. Each element may be another list of named elements (but these nested lists are not allowed to be lists themselves)}
}

\value{
Raster* object or list
}


\examples{ 
r <- raster(nc=10, nr=10)
r[] <- 1:ncell(r)

m <- list(wave=list(a=1, b=2, c=c('cool', 'important')),
that=list(red='44', blue=1:5,
days=as.Date(c('2014-1-15','2014-2-15'))), this='888 miles from here
to the there and back.', today=NA)

metadata(r) <- m
x <- writeRaster(r, 'test.grd', overwrite=TRUE)
metax <- metadata(x)

identical(metax, m)

\dontrun{
# nested too deep
badmeta <- list(wave=list(a=1, b=2, c='important'),
that=list(red='44', blue=list(bad=124)))
metadata(r) <- badmeta

# missing names
badmeta <- list(wave=list(1, 2, c='important'), that=list(red='44', blue=14), this='8m')
metadata(r) <- badmeta

# matrix not allowed
badmeta <- list(wave=list(a=1, b=matrix(1:4, ncol=2), c='important'),
that=list(red='44', blue=124))
metadata(r) <- badmeta
}
}

\keyword{ spatial }

