\name{writeRaster}

\alias{writeRaster,RasterLayer-method}
\alias{writeRaster,RasterStack-method}
\alias{writeRaster,RasterBrick-method}

\alias{writeRaster}

\title{Write raster data to a file}

\description{
Write raster values to a file. Either as binary ('raster') format, Ascii (ESRI Arc Ascii) format, and other GDAL supported formats. 
You can write an entire raster dataset at once, or by row.
}

\usage{
writeRaster(x, ...)
}

\arguments{
  \item{x}{Raster* object}
  \item{...}{Additional arguments. See below, under Methods}    
}

\section{Methods}{
\describe{
\bold{Full function call when \code{x} is a RasterLayer object}

\code{writeRaster(x, filename='', assign=FALSE, filetype, datatype, overwrite) }
 
\tabular{rll}{
\tab \code{x} \tab RasterLayer object \cr
\tab \code{assign} \tab Logical. If \code{TRUE} the function does not return a new object but overwrites the existing object \cr
\tab \code{filename} \tab Character. output filename \cr
\tab \code{filetype} \tab Character. output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeFormats}} \cr
\tab \code{datatype} \tab Character. output data type; see \code{\link[raster]{dataType}} \cr
\tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
}


\bold{Full function call when \code{x} is a RasterBrick or RasterStack object}

\code{writeRaster(x, filename='', bandorder, filetype, datatype, overwrite) }
 
\tabular{rll}{
\tab \code{x} \tab RasterLayer object \cr
\tab \code{assign} \tab Logical. If \code{TRUE} the function does not return a new object but overwrites the existing object \cr
\tab \code{filename} \tab Character. Output filename. Can be missing if \code{x} is a RasterBrick that has a filename \cr
\tab \code{bandorder} \tab Character. 'BIL', 'BIP', or 'BSQ' \cr
\tab \code{filetype} \tab Character. output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeFormats}} \cr
\tab \code{datatype} \tab Character. output data type; see \code{\link[raster]{dataType}} \cr
\tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
}
}
}

\details{
Values in a \code{RasterLayer} object are written to a file. Relatively small datasets can be loaded into memory, manipulated, and saved to disk in their entirety. Large datasets can be read and written row by row. 
When writing by row, you must write all rows, and you must write them in sequence. Start at row=1, and end at row=nrow(raster). You cannot overwrite a single row in an existing raster file.

multi-band files (i.e. files saved from RasterStack or RasterBrick objects) can be saved as 

BIL = 'Bands Interleaved by Line'

BIP = 'Bands Interleaved by Pixels'

BSQ = 'Bands SeQuential'


Note that the values of a RasterBrick and RasterLayer objects can be written to a single file in a row by row fashion; but the values of a RasterStack can only be saved to a file in one go.  


See \code{writeFormats} for supported file types ("formats", "drivers"); the rgdal package is needed for this function, exxcept for the 'raster', ascii, and 'CDF' formats.

Supported file types include:

\tabular{lllr}{
	\tab \bold{File type} \tab \bold{Long name} \tab \bold{Multiband support} \cr
    \tab \code{raster} \tab 'Native' raster package format \tab Yes \cr
    \tab \code{ascii}  \tab ESRI Ascii \tab No \cr
    \tab \code{CDF}  \tab netCDF (requires RNetCDF) \tab pending \cr
    \tab \code{GTiff}  \tab GeoTiff (requires rgdal) \tab pending  \cr
    \tab \code{ENVI}   \tab ENVI .hdr Labelled \tab pending  \cr
    \tab \code{EHdr}   \tab ESRI .hdr Labelled \tab pending  \cr
    \tab \code{ERS}  \tab ERMapper .ers Labelled \tab pending  \cr
    \tab \code{HFAErdas}  \tab Imagine Images (.img) \tab pending  \cr
    \tab \code{ILWIS}   \tab ILWIS Raster Map \tab pending  \cr
    \tab \code{PAux}   \tab PCI .aux Labelled \tab pending  \cr
    \tab \code{PCIDSK}   \tab PCIDSK Database File \tab pending  \cr
    \tab \code{RST}  \tab  Idrisi Raster A.1 \tab  No  \cr
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
\keyword{ methods }
