\name{writeRaster}

\alias{writeRaster}

\title{Write raster data to a file}

\description{
Write raster values to a file. Either as binary ('raster') format, Ascii (ESRI Arc Ascii) format, and other GDAL supported formats. 
You can write an entire raster dataset at once, or by row.
}

\usage{
writeRaster(raster, filetype='raster', filename='', overwrite=FALSE, assign=FALSE)
}

\arguments{
  \item{raster}{RasterLayer object}
  \item{filetype}{either 'raster', 'ascii', or a supported GDAL 'driver' name (see below)}
  \item{filename}{Character. If omitted, the filename of the RasterLayer object is used (if it has one)}
  \item{overwrite}{Logical. If \code{TRUE} existing files are overwritten}
  \item{assign}{Logical. If \code{TRUE} the function does not return a new object but overwrites the existing object} 
}

\details{
Values in a \code{RasterLayer} object are written to a file. Relatively small datasets can be loaded into memory, manipulated, and saved to disk in their entirety. Large datasets can be read and written row by row. 
When writing by row, you must write all rows, and you must write them in sequence. Start at row=1, and end at row=nrow(raster). You cannot overwrite a single row in an existing raster file.
See \code{writeFormats} for supported file types ("formats", "drivers"); the rgdal package is needed for this function, exxcept for the 'raster', ascii, and 'CDF' formats.

Supported file types include:

\tabular{rll}{
	\tab \bold{file type} \tab \bold{Long name} \cr
    \tab \code{raster} \tab 'Native' raster package format \cr
    \tab \code{ascii}  \tab ESRI Ascii \cr
    \tab \code{CDF}  \tab netCDF (requires RNetCDF)\cr
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

\value{
This function is used for the side-effect of writing values to a file.
}

\author{Robert J. Hijmans}

\seealso{ writeFormats \code{\link[raster]{writeFormats}} }  

\examples{ 
rst <- raster(system.file("external/test.ag", package="sp"))
 
# read all data
rst <- readAll(rst)

# write all to a new binary file
filename(rst) <- "binall.grd"
rst <- writeRaster(rst, overwrite=TRUE)

# equivalent to:
writeRaster(rst, overwrite=TRUE, assign=TRUE)


# write all to a new ERDAS .img file
filename(rst) <- "binall.img"
rst <- writeRaster(rst, filetype="HFA", overwrite=TRUE)

 
# write all to an integer binary file
filename(rst) <- "binallint.grd"
dataType(rst) <- "INT4S"
rst <- writeRaster(rst, overwrite=TRUE)
 
# write all to ascii file
filename(rst) <- "ascall.asc"
rst <- writeRaster(rst, filetype='ascii', overwrite=TRUE)
 
# read and write row by row; write to ascii file
rst <- raster(system.file("external/test.ag", package="sp"))
ascras <- raster(rst)
filename(ascras) <- "ascrow.asc"
for (r in 1:nrow(rst)) {
	rst <- readRow(rst, r)
	v <- values(rst) * 10
	ascras <- setValues(ascras, v, r)
	ascras <- writeRaster(ascras, filetype='ascii', overwrite=TRUE) 
}
	
# read and write row by row; write to binary file
binras <- raster(rst, filename="binbyrow")
for (r in 1:nrow(rst)) {
	rst <- readRow(rst, r)
	binras <- setValues(binras, values(rst), r)
	binras <- writeRaster(binras, overwrite=TRUE) 
}

# read and write row by row; write to GeoTiff binary file
binras <- raster(rst)
filename(binras) <- "binbyrow2.tif"
for (r in 1:nrow(rst)) {
	rst <- readRow(rst, r)
	binras <- setValues(binras, values(rst), r)
	binras <- writeRaster(binras, filetype="GTiff", overwrite=TRUE) 
}

# write to netcdf 
if (require(RNetCDF)) {	
   rst <- readAll(rst)
   writeRaster(rst, filetype="CDF", filename="netCDF.nc", overwrite=TRUE)   
}

}

\keyword{ spatial }
