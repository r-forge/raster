\name{write}
\alias{write.raster}
\alias{write.row}
\alias{write.ascii}
\alias{write.import}
\alias{write.export}
\alias{writeHdr}

\title{ Write raster data to a file }
\description{
  Write raster data to a file. Either as binary ('raster') format or Ascii (ESRI Arc Ascii) format. 
}
\usage{
write.raster(raster, type="grd", INT=FALSE, overwrite=FALSE)
write.row(raster, overwrite=FALSE)
write.ascii(raster, overwrite=FALSE)
write.import(raster, outfile, overwrite=FALSE)
write.export(raster, outfile, filetype='ascii', overwrite=FALSE)
writeHdr(raster)
}

\arguments{
  \item{raster}{ RasterLayer object }
  \item{type} { type of binary file. Currently only "grd" is supported }
  \item{INT} { Logical. If \code{TRUE}, values will be saved as Integers  }
  \item{overwrite} { Logical. If \code{TRUE} existing files are overwritten }
  \item{outfile}{ The file resulting from import or export }
  \item{filetype}{ The type of file to export to }
}

\details{
	data in a \code{RasterLayer} object is written to a file. Small datasets can be loaded into memory manipulated, 
	and saved to disk again. For large datasets, you can read and write by row. writeHdr saves the header file only. You should normally not directly call this.
}

\author{Robert J. Hijmans \email{r.hijmans@gmail.com}}

\seealso{ \code{\link[rgdal]{rgdal}} }
\examples{ 

 rs <- raster.from.file(system.file("external/test.ag", package="sp"))
 
\dontrun{
#read all data
 rs <- readAll(rs)

# write all to a new binary file
 rs <- set.filename(rs, "binall.grd")
 rs <- write.raster(rs, overwrite=TRUE)
 
# write all to integer binary file
 rs <- set.filename(rs, "binallint.grd")
 rs <- set.datatype(rs, "integer")
 rs <- write.raster(rs, overwrite=TRUE)
 
# write all to ascii file
 rs <- set.filename(rs, "ascall.asc")
 rs <- write.ascii(rs, overwrite=TRUE)
 
# read and write row by row; write to ascii file
 rs <- raster.from.file(system.file("external/test.ag", package="sp"))
 ascrow <- set.filename(rs, "ascrow.asc")
 for (r in 1:nrow(rs)) {
	rs <- readRow(rs, r)
	ascrow <- set.values.row(ascrow, values(rs), r)
	ascrow <- write.ascii(ascrow, overwrite=TRUE) 
 }
	
# read and write row by row; write to binary file
 binras <- set.raster(rs, "binbyrow.grd")
 for (r in 1:nrow(rs)) {
#	binras <- set.values.row(binras, values(readRow(rs, r)), r)
# a shortcut for this:	
    binras@data <- readRow(rs, r)@data
	binras <- write.row(binras, overwrite=TRUE) }
}
}

\keyword{ spatial }
