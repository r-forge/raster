\name{headerFiles}

\alias{writeHeader}
\alias{worldFile}

\title{Header files}

\description{
Write header files to use together with raster binary files to read the data in other applications.
}

\usage{
writeHeader(raster, type) 
worldFile(raster, extension=".wld") 
}

\arguments{
  \item{raster}{ RasterLayer object }
  \item{type}{ Type of header file: 'BIL', 'ENVI', 'ErdasRaw', 'raster' }
  \item{extension}{ extension of header file. Only implemented for worldfile. }
}

\details{
You can use saveAs to save a existing file in another format. But if you have a file in a 'raster' format (or similar), 
you can also only export a header file, and use the data file that already exists.\
writeHeader writes an ENVI, or BIL header (.hdr) file; or an Erdas Raw (.raw) header file. This (hopefully) allows for 
reading the binary data ("raster.gri") in other programs such as ENVI or ArcMap.

A "worldfile", is an ESRI "world file", that can be used to georeference images.
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{saveAs}}, \code{\link[raster]{writeRaster}}, \code{\link[rgdal]{writeGDAL}} }

\examples{ 

r <- raster(system.file("external/test.ag", package="sp"))
r <- readAll(r)
filename(r) <- 'export.grd'
r <- writeRaster(r)
writeHeader(r, type="ENVI") 

}

\keyword{ spatial }
