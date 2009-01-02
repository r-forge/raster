\name{write}
\alias{write.raster}
\alias{write.row}
\alias{write.ascii}
\alias{import}
\alias{export}
\alias{grdToBil}
\alias{writeHdr}
\alias{writeENVIHdr}
\alias{writeErdasRawHdr}
\alias{writeBilHdr}
\alias{writeWorldFile}

\title{ Write raster data to a file }
\description{
  Write raster data to a file. Either as binary ('raster') format or Ascii (ESRI Arc Ascii) format. 
  You can also export an existing raster file to an ASCII or BIL file. Or create an ENVI header for it.
}
\usage{
write.raster(raster, type="grd", ForceIntOutput=FALSE, overwrite=FALSE)
write.row(raster, overwrite=FALSE)
write.ascii(raster, overwrite=FALSE)
import(raster, outfile, overwrite=FALSE)
export(raster, outfile="", filetype='ascii', overwrite=FALSE)
grdToBil(raster, outfile="", keepGRD=TRUE, overwrite=FALSE) 
writeHdr(raster)
writeENVIHdr(raster)
writeErdasRawHdr(raster)
writeBilHdr(raster)
writeWorldFile(raster, extension=".world")
}

\arguments{
  \item{raster}{ RasterLayer object }
  \item{type} { type of binary file. Currently only "grd" is supported }
  \item{ForceIntOutput} { Logical. If \code{TRUE}, values will be saved as Integers  }
  \item{overwrite} { Logical. If \code{TRUE} existing files are overwritten }
  \item{outfile}{ The file resulting from import or export }
  \item{filetype}{ The type of file to export to }
  \item{keepGRD} { Logical. If \code{FALSE} the existing raster file GRD/GRI will not be kept after exporting to BIL format}
  \item{extension}{ File extension for the world file }
 }

\details{
	data in a \code{RasterLayer} object is written to a file. Small datasets can be loaded into memory manipulated, 
	and saved to disk again. For large datasets, you can read and write by row. 
	writeHdr saves the header file only. You should normally not directly call this function.
	writeENVIHdr writes an ENVI header (.hdr) file. This (hopefully) allows for reading the binary data ("raster.gri") in other programs such as ENVI ArcMap.
	writeErdasRawHdr writes an header(.raw) file. This (hopefully) allows for reading the binary data ("raster.gri") in other programs such as Erdas and ArcMap.
	writeBILHdr writes a "bil" format header (.hdr) file
	writeWorldFile writes an ESRI "world" file, that is sometimes used to georeference images.
}

\author{Robert J. Hijmans \email{r.hijmans@gmail.com}}

\seealso{ \code{\link[rgdal]{rgdal}} }
\examples{ 

 rs <- rasterFromFile(system.file("external/test.ag", package="sp"))
 
\dontrun{
#read all data
 rs <- readAll(rs)

# write all to a new binary file
 rs <- setFilename(rs, "binall.grd")
 rs <- write.raster(rs, overwrite=TRUE)
 
# write all to integer binary file
 rs <- setFilename(rs, "binallint.grd")
 rs <- setDatatype(rs, "integer")
 rs <- write.raster(rs, overwrite=TRUE)
 
# write all to ascii file
 rs <- setFilename(rs, "ascall.asc")
 rs <- write.ascii(rs, overwrite=TRUE)
 
# read and write row by row; write to ascii file
 rs <- rasterFromFile(system.file("external/test.ag", package="sp"))
 ascrow <- setFilename(rs, "ascrow.asc")
 for (r in 1:nrow(rs)) {
	rs <- readRow(rs, r)
	ascrow <- setValuesRow(ascrow, values(rs), r)
	ascrow <- write.ascii(ascrow, overwrite=TRUE) 
 }
	
# read and write row by row; write to binary file
 binras <- setRaster(rs, "binbyrow.grd")
 for (r in 1:nrow(rs)) {
#	binras <- setValuesRow(binras, values(readRow(rs, r)), r)
# a shortcut for this:	
    binras@data <- readRow(rs, r)@data
	binras <- write.row(binras, overwrite=TRUE) }
}
}

\keyword{ spatial }
