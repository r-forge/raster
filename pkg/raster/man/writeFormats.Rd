\name{writeFormats}

\alias{writeFormats}

\title{List supported file types for writing RasterLayer values to disk}

\description{
xxx
}

\usage{
  writeFormats()
}


\details{
	writeFormats returns a matrix of the file formats (the "drivers") that are supported.

Supported formats include:

\tabular{rll}{
	\tab \code{\bold{format}} \tab \bold{Long name} \cr
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
    \tab \code{RST}  \tab  Idrisi Raster A.1
}

}

\author{Robert J. Hijmans}

\seealso{ \code{\link[rgdal]{readGDAL}} }  

\examples{ 
rs <- rasterFromFile(system.file("external/test.ag", package="sp"))
 
#read all data
rs <- readAll(rs)

# write all to a new binary file
rs <- setFilename(rs, "binall.grd")
rs <- writeRaster(rs, overwrite=TRUE)

# write all to a new ERDAS .img file
rs <- setFilename(rs, "binall.img")
rs <- writeRaster(rs, filetype="HFA", overwrite=TRUE)

 
# write all to integer binary file
rs <- setFilename(rs, "binallint.grd")
rs <- setDatatype(rs, "INT4S")
rs <- writeRaster(rs, overwrite=TRUE)
 
# write all to ascii file
rs <- setFilename(rs, "ascall.asc")
rs <- writeRaster(rs, filetype='ascii', overwrite=TRUE)
 
# read and write row by row; write to ascii file
rs <- rasterFromFile(system.file("external/test.ag", package="sp"))
ascras <- setRaster(rs, filename="ascrow.asc")
for (r in 1:nrow(rs)) {
	rs <- readRow(rs, r)
	v <- values(rs) * 10
	ascras <- setValues(ascras, v, r)
	ascras <- writeRaster(ascras, filetype='ascii', overwrite=TRUE) 
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
	binras <- writeRaster(binras, filetype="GTiff", overwrite=TRUE) 
}
  
 
}

\keyword{ spatial }
