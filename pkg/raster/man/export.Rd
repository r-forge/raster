\name{export}

\alias{import}
\alias{export}
\alias{exportGDAL}
\alias{writeHeader}
\alias{writeWorldfile}

\title{ Write raster data to a file }
\description{
  Export an existing raster file to an ASCII or BIL file. Or to many other formats via exportGDAL
  Or create header files associated with other formats for use in different software (use ENVI or ErdasRaw for ArcGIS).
}
\usage{
export(raster, filename="", filetype="ascii", keepGRD=TRUE, overwrite=FALSE)
exportGDAL(raster, filename, gdalfiletype="GTiff", overwrite=FALSE, ForceIntOutput = FALSE)
writeHeader(raster, type) 
writeWorldfile(raster, extension=".wld") 
import(raster, filename="", overwrite=FALSE)
}

\arguments{
  \item{raster}{ RasterLayer object }
  \item{overwrite}{ Logical. If \code{TRUE} existing files are overwritten }
  \item{filename}{ The file resulting from import or export }
  \item{filetype}{ The type of file to export to ('ascii' or 'bil')}
  \item{gdalfiletype}{ GDAL file type (driver) name. See below for list of supported file types }
  \item{ForceIntOutput}{save as integer}
  \item{keepGRD}{ Logical. If \code{FALSE} the existing raster file (internal format) will not be kept after exporting; only applied to other formats that can use the same binary format. }
  \item{type}{ Type of header file: 'BIL', 'ENVI', 'ErdasRaw', 'raster' }
  \item{extension}{ extension of header file. Only implemented for worldfile. }
}

\details{
	data in a \code{RasterLayer} object is written to a file. Small datasets can be loaded into memory manipulated, 
	and saved to disk again. For large datasets, you can read and write by row. 
	writeHeader writes an ENVI, or BIL header (.hdr) file; or an Erdas Raw (.raw) header file. This (hopefully) allows for reading the binary data ("raster.gri") in other programs such as ENVI ArcMap.
	if format = "worldfile", the function writes an ESRI "world" file, that is sometimes used to georeference images.
	
}

\author{Robert J. Hijmans \email{r.hijmans@gmail.com}}

\seealso{ \code{\link[rgdal]{rgdal}} }
\examples{ 

rs <- rasterFromFile(system.file("external/test.ag", package="sp"))
rsimport <- import(rs, filename="imported", overwrite=TRUE)
rsexport <- export(rsimport, filename="exported", filetype='ascii', overwrite=TRUE)
writeHeader(rsexport, type="ENVI") 

rsimp <- import(rs, filename="imported", overwrite=TRUE)

}

\keyword{ spatial }
