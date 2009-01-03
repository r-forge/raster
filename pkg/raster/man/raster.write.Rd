\name{write}

\alias{writeValues}
\alias{writeAscii}
\alias{import}
\alias{export}
\alias{writeHeader}
\alias{writeOtherHeader}

\title{ Write raster data to a file }
\description{
  Write raster values to a file. Either as binary ('raster') format or Ascii (ESRI Arc Ascii) format. 
  You can also export an existing raster file to an ASCII or BIL file. Or create an header files for other formats it.
}
\usage{
writeValues(raster, overwrite=FALSE)
writeAscii(raster, filename, overwrite=FALSE)
import(raster, filename="", overwrite=FALSE)
export(raster, filename="", filetype='ascii', keepGRD=TRUE, overwrite=FALSE)
writeHeader(raster)
writeOtherHeader(raster, format="BIL") 
}

\arguments{
  \item{raster}{ RasterLayer object }
  \item{overwrite} { Logical. If \code{TRUE} existing files are overwritten }
  \item{filename}{ The file resulting from import or export }
  \item{filetype}{ The type of file to export to ('ascii' or 'bil')}
  \item{keepGRD} { Logical. If \code{FALSE} the existing raster file (internal format) will not be kept after exporting; only applied to other formats that can use the same binary format. }
  \item{format}{ Type of header file. Either: 'BIL', 'ENVI', ErdasRaw', or 'worldfile' }
}

\details{
	data in a \code{RasterLayer} object is written to a file. Small datasets can be loaded into memory manipulated, 
	and saved to disk again. For large datasets, you can read and write by row. 
	writeHeader saves the header file only. You should normally not directly call this function; but may want to do this to save some of your own parameters to the header file.
	writeOtherHeader writes an ENVI, or BIL header (.hdr) file; or an Erdas Raw (.raw) header file. This (hopefully) allows for reading the binary data ("raster.gri") in other programs such as ENVI ArcMap.
	if format = "worldfile", the function writes an ESRI "world" file, that is sometimes used to georeference images.
}

\author{Robert J. Hijmans \email{r.hijmans@gmail.com}}

\seealso{ \code{\link[rgdal]{rgdal}} }
\examples{ 

 rs <- rasterFromFile(system.file("external/test.ag", package="sp"))
 
#read all data
 rs <- readAll(rs)

# write all to a new binary file
 rs <- setFilename(rs, "binall.grd")
 rs <- writeValues(rs, overwrite=TRUE)
 
# write all to integer binary file
 rs <- setFilename(rs, "binallint.grd")
 rs <- setDatatype(rs, "integer")
 rs <- writeValues(rs, overwrite=TRUE)
 
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
	binras <- writeValues(binras, overwrite=TRUE) 
 }

rsimport <- import(rs, filename="imported", overwrite=TRUE)
rsexport <- export(rsimport, filename="exported", filetype='ascii', overwrite=TRUE)
writeOtherHeader(rsexport, format="ENVI") 

}

\keyword{ spatial }
