\name{write}

\alias{writeRaster}
\alias{rasterFormats}

\title{Write raster data to a file}

\description{
  Write raster values to a file. Either as binary ('raster') format, Ascii (ESRI Arc Ascii) format, and other GDAL supported formats. 
  You can write an entire raster dataset at once, or by row.
}

\usage{
  writeRaster(raster, format='raster', overwrite=FALSE)
  rasterFormats()
}

\arguments{
  \item{raster}{RasterLayer object}
  \item{format}{either 'raster', 'ascii' or a supported GDAL 'driver' name}
  \item{overwrite}{Logical. If \code{TRUE} existing files are overwritten}
}

\details{
	Values in a \code{RasterLayer} object are written to a file. Relatively small datasets can be loaded into memory, manipulated, and saved to disk in their entirety. 
	Large datasets can be read and written row by row. 
	When writing by row, you must write all rows, and you must write them in sequence. Start at row=1, and end at row=nrow(raster). You cannot overwrite a single row in an existing raster file.
	rasterFormats returns a matrix of the file formats (the "drivers") that are supported.

Supported formats include 
	\item{Format}{Long name}
	\item{raster}{'Native' raster package format}
	\item{ascii}{Arc Ascii format}
	\item{ENVI}{ENVI .hdr Labelled}
	\item{EHdr}{ESRI .hdr Labelled}
	\item{ERS}{ERMapper .ers Labelled}
	\item{GTiff}{GeoTiff}
	\item{HFA}{Erdas Imagine Images (.img)}
	\item{ILWIS}{ILWIS Raster Map}
	\item{PAux}{PCI .aux Labelled}
	\item{PCIDSK}{PCIDSK Database File}
	\item{RST}{Idrisi Raster A.1}
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

# write all to a new ERDAS .img file
 rs <- setFilename(rs, "binall.img")
 rs <- writeRaster(rs, format="HFA", overwrite=TRUE)

 
# write all to integer binary file
 rs <- setFilename(rs, "binallint.grd")
 rs <- setDatatype(rs, "integer")
 rs <- writeRaster(rs, overwrite=TRUE)
 
# write all to ascii file
 rs <- setFilename(rs, "ascall.asc")
 rs <- writeRaster(rs, format='ascii', overwrite=TRUE)
 
# read and write row by row; write to ascii file
 rs <- rasterFromFile(system.file("external/test.ag", package="sp"))
 ascras <- setRaster(rs, filename="ascrow.asc")
 for (r in 1:nrow(rs)) {
	rs <- readRow(rs, r)
	v <- values(rs) * 10
	ascras <- setValues(ascras, v, r)
	ascras <- writeRaster(ascras, format='ascii', overwrite=TRUE) 
 }
	
# read and write row by row; write to binary file
 binras <- setRaster(rs, "binbyrow")
 for (r in 1:nrow(rs)) {
	rs <- readRow(rs, r)
	binras <- setValues(binras, values(rs), r)
	binras <- writeRaster(binras, overwrite=TRUE) 
 }

# read and write row by row; write to GeoTiff binary file
 binras <- setRaster(rs, "binbyrow")
 for (r in 1:nrow(rs)) {
	rs <- readRow(rs, r)
	binras <- setValues(binras, values(rs), r)
	binras <- writeRaster(binras, format="GTiff", overwrite=TRUE) 
 }
  
 
}

\keyword{ spatial }
