\name{write}

\alias{writeRaster}
\alias{writeAscii}
\alias{import}

\title{ Write raster data to a file }
\description{
  Write raster values to a file. Either as binary ('raster') format or Ascii (ESRI Arc Ascii) format. 
  You can also export an existing raster file to an ASCII or BIL file. 
  Or create header files associated with other formats for use in different software (use ENVI or ErdasRaw for ArcGIS).
}
\usage{
writeRaster(raster, overwrite=FALSE)
writeAscii(raster, filename, overwrite=FALSE)
import(raster, filename="", overwrite=FALSE)
}

\arguments{
  \item{raster}{ RasterLayer object }
  \item{overwrite}{ Logical. If \code{TRUE} existing files are overwritten }
  \item{filename}{ The file resulting from import or export }
}

\details{
	data in a \code{RasterLayer} object is written to a file. Small datasets can be loaded into memory manipulated, 
	and saved to disk again. For large datasets, you can read and write by row. 
}

\author{Robert J. Hijmans \email{r.hijmans@gmail.com}}

\seealso{ \code{\link[rgdal]{rgdal}} }
\examples{ 

 rs <- rasterFromFile(system.file("external/test.ag", package="sp"))
 
#read all data
 rs <- readAll(rs)

# write all to a new binary file
 rs <- setFilename(rs, "binall.grd")
 rs <- writeRaster(rs, overwrite=TRUE)
 
# write all to integer binary file
 rs <- setFilename(rs, "binallint.grd")
 rs <- setDatatype(rs, "integer")
 rs <- writeRaster(rs, overwrite=TRUE)
 
# write all to ascii file
 rsascii <- writeAscii(rs, "ascall.asc", overwrite=TRUE)
 
# read and write row by row; write to ascii file
 rs <- rasterFromFile(system.file("external/test.ag", package="sp"))
 for (r in 1:nrow(rs)) {
	rs <- readRow(rs, r)
	ascrow <- writeAscii(rs, "ascrow.asc", overwrite=TRUE) 
 }
	
# read and write row by row; write to binary file
 binras <- setRaster(rs, "binbyrow")
 for (r in 1:nrow(rs)) {
	rs <- readRow(rs, r)
	binras <- setValues(binras, values(rs), r)
	binras <- writeRaster(binras, overwrite=TRUE) 
 }

rsimport <- import(rs, filename="imported", overwrite=TRUE)

}

\keyword{ spatial }
