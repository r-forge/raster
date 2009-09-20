\name{writeStack}

\alias{writeStack}
\alias{writeBrick}

\title{Write RasterStack or RasterBrick to file}

\description{
Write the values of a RasterStack or RasterBrick object to a multi-band RasterFile.
}

\usage{
  writeStack(rstack, filename, bandorder='BIL', ...)
  writeBrick(brick, filename, bandorder='BIL', ...)
}

\arguments{
  \item{rstack}{RasterStack object} 
  \item{brick}{RasterBrick object} 
  \item{filename}{character. output filename}
  \item{bandorder}{BIL, BIP, or BSQ} 
  \item{...}{additional arguments. See Details.}  
}

\details{
BIL = 'Bands Interleaved by Line'

BIP = 'Bands Interleaved by Pixels'

BSQ = 'Bands SeQuential'

The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab overwrite \tab if TRUE, "filename" will be overwritten if it exists \cr
  \tab filetype \tab output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeRaster}} \cr
  \tab datatype \tab output data type; see \code{\link[raster]{dataType}} \cr
 }

}


\author{Robert J. Hijmans}

 
\keyword{ spatial }

