\name{saveAs}

\alias{saveAs}

\title{Save a raster file to a different format}

\description{
Save a raster file to a file of another type (format). 
}

\usage{
saveAs(raster, filename, filetype, datatype, overwrite, progress, result=TRUE)
}

\arguments{
  \item{raster}{RasterLayer object}
  \item{filename}{output filename}  
  \item{filetype}{either 'raster', 'ascii' or a supported GDAL 'driver' name (see below)}
  \item{datatype}{output data type; see below}
  \item{overwrite}{Logical. If \code{TRUE} existing files are overwritten}
  \item{result}{Logical. If \code{TRUE} a new RasterLayer object is returned, else the function returns nothing} 
  \item{progress}{Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""}
 
}

\value{
A RasterLayer object or nothing \code{result=FALSE}. This function is used for its side-effect of writing a file to disk.
}

\details{

Supported datatypes include:

\tabular{rll}{
	\tab \bold{Format} \tab \bold{Long name} \cr
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

see \code{writeFormats} for the complete list

}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{writeRaster}} }  

\examples{ 
r1 <- raster(system.file("external/test.ag", package="sp"))
r2 <- saveAs(r1, 'test.tif', filetype='GTiff')

}

\keyword{ spatial }
