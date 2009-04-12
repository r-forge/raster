\name{writeStack}

\alias{writeStack}

\title{Write RasterStack}

\description{

NOT IMPLEMENTED YET

values of a RasterStack to a multi-band RasterFile
}

\usage{
  writeStack(rstack, bandorder='BIL', filename='', filetype='raster', datatype='FLT4S', overwrite=FALSE)
}

\arguments{
  \item{rstack}{RasterStack object} 
  \item{bandorder}{BIL, BIP, or BSQ} 
  \item{filename}{character. output filename}
  \item{overwrite}{logical. If \code{TRUE}, "filename" will be overwritten if it exists}
\item{filetype}{character. output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{rasterFormats}}}
\item{datatype}{character. output data type; see \code{\link[raster]{setDatatype}}}
}

\details{
xxx
}


\author{Robert J. Hijmans}

 
\keyword{ spatial }

