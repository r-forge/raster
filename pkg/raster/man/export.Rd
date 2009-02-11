\name{header files}

\alias{writeHeader}
\alias{worldFile}

\title{ Write raster data to a file }
\description{
  Or create header files associated with other formats for use in different software (use ENVI or ErdasRaw for ArcGIS).
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
	You can use writeRaster to save a existing file in another format. But if you have a file in a 'raster' format (or similar), you can also only export a header file, and use the data file that already exists.
	writeHeader writes an ENVI, or BIL header (.hdr) file; or an Erdas Raw (.raw) header file. This (hopefully) allows for reading the binary data ("raster.gri") in other programs such as ENVI ArcMap.
	if format = "worldfile", the function writes an ESRI "world" file, that is sometimes used to georeference images.	
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[rgdal]{writeGDAL}} }
\examples{ 

rs <- rasterFromFile(system.file("external/test.ag", package="sp"))
rs <- readAll(rs)
rs <- setFilename(rs, 'export.grd')
rs <- writeRaster(rs)
writeHeader(rs, type="ENVI") 

}

\keyword{ spatial }
