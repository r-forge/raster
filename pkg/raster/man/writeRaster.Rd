\name{writeRaster}

\alias{writeRaster}

\title{Write raster data to a file}

\description{
Write raster values to a file. Either as binary ('raster') format, Ascii (ESRI Arc Ascii) format, and other GDAL supported formats. 
You can write an entire raster dataset at once, or by row.
}

\usage{
writeRaster(raster, filetype='raster', overwrite=FALSE)
}

\arguments{
  \item{raster}{RasterLayer object}
  \item{filetype}{either 'raster', 'ascii' or a supported GDAL 'driver' name (see below)}
  \item{overwrite}{Logical. If \code{TRUE} existing files are overwritten}
}

\details{
Values in a \code{RasterLayer} object are written to a file. Relatively small datasets can be loaded into memory, manipulated, and saved to disk in their entirety. 
Large datasets can be read and written row by row. 
When writing by row, you must write all rows, and you must write them in sequence. Start at row=1, and end at row=nrow(raster). You cannot overwrite a single row in an existing raster file.
See \code{writeFormats} for supported file types ("formats", "drivers").

Supported file types include:

\tabular{rll}{
	\tab \bold{file type} \tab \bold{Long name} \cr
    \tab \code{raster} \tab 'Native' raster package format \cr
    \tab \code{ascii}  \tab ESRI Ascii \cr
    \tab \code{ENVI}   \tab ENVI .hdr Labelled \cr
    \tab \code{EHdr}   \tab ESRI .hdr Labelled \cr
    \tab \code{ERS}  \tab ERMapper .ers Labelled \cr
    \tab \code{GTiff}  \tab GeoTiff \cr
    \tab \code{HFAErdas}  \tab Imagine Images (.img) \cr
    \tab \code{ILWIS}   \tab ILWIS Raster Map \cr
    \tab \code{PAux}   \tab PCI .aux Labelled \cr
    \tab \code{PCIDSK}   \tab PCIDSK Database File \cr
    \tab \code{RST}  \tab  Idrisi Raster A.1 \cr
}
}

\author{Robert J. Hijmans}

\seealso{ writeFormats \code{\link[raster]{writeFormats}} }  

\examples{ 
rs <- raster(system.file("external/test.ag", package="sp"))
 
#read all data
rs <- readAll(rs)

# write all to a new binary file
filename(rs) <- "binall.grd"
rs <- writeRaster(rs, overwrite=TRUE)

# write all to a new ERDAS .img file
filename(rs) <- "binall.img"
rs <- writeRaster(rs, filetype="HFA", overwrite=TRUE)

 
# write all to integer binary file
filename(rs) <- "binallint.grd"
rs <- setDatatype(rs, "INT4S")
rs <- writeRaster(rs, overwrite=TRUE)
 
# write all to ascii file
filename(rs) <- "ascall.asc"
rs <- writeRaster(rs, filetype='ascii', overwrite=TRUE)
 
# read and write row by row; write to ascii file
rs <- raster(system.file("external/test.ag", package="sp"))
ascras <- raster(rs, filename="ascrow.asc")
for (r in 1:nrow(rs)) {
	rs <- readRow(rs, r)
	v <- values(rs) * 10
	ascras <- setValues(ascras, v, r)
	ascras <- writeRaster(ascras, filetype='ascii', overwrite=TRUE) 
}
	
# read and write row by row; write to binary file
binras <- raster(rs, "binbyrow")
for (r in 1:nrow(rs)) {
	rs <- readRow(rs, r)
	binras <- setValues(binras, values(rs), r)
	binras <- writeRaster(binras, overwrite=TRUE) 
}

# read and write row by row; write to GeoTiff binary file
binras <- raster(rs, "binbyrow")
for (r in 1:nrow(rs)) {
	rs <- readRow(rs, r)
	binras <- setValues(binras, values(rs), r)
	binras <- writeRaster(binras, filetype="GTiff", overwrite=TRUE) 
}
}

\keyword{ spatial }
