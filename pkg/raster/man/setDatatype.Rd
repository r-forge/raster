\name{set data tyoe}
\alias{setDatatype}

\title{ Set data type }

\description{
Use these functions to set the datatype properties of a RasterLayer object. The datatype determines how values are written to disk. 
}

\usage{
setDatatype(raster, datatype, datasize=4, signed=TRUE)
}

\arguments{
  \item{raster}{ A \code{RasterLayer} object }
  \item{datatype}{ the type of data ('numeric' or 'integer' that will be written to disk }
  \item{datasize}{ The size, in number of bytes, that wil be written to disk. typically 4 for integer, and 8 for numeric datatype }
  \item{signed}{ logical}
}

\details{
While you can access the slots of the object directly, you would do that at your own peril because the value of one raster property can affect values in other properties, and changing slot values may thus break the integrity of the objects. Using slots directly may also lead to code to break in the future if underlying class definitions change. 


The following datatype/size/signed combinations are available.

datatype, size, signed: range of possible values

'logical', 1: FALSE (0) to TRUE (1)

'integer', 1, signed: -127 to 127

'integer', 1, unsigned: 0 to 255

'integer', 2, signed: -32,767 to 32,767

'integer', 2, unsigned: 0 to 65,534

'integer', 4, signed: -2,147,483,647 to 2,147,483,647

'integer', 4, unsigned: 0 to 4,294,967,294

'integer', 8, signed: -9,223,372,036,854,775,807 to 9,223,372,036,854,775,807

'integer', 8, unsigned: 0 to 18,446,744,073,709,551,614

'numeric', 4, signed:

'numeric', 8, signed:


For all integer types, except the single byte types, the lowest (signed) or highest (unsigned) value is used to store NA. Single byte files do not have an NA.
Logical values are stored as signed single byte integers, they do have an NA value (-127)
}

\value{
	a Raster* object
}
\author{ Robert J. Hijmans }

\examples{ 
r <- raster()
# 2 byte signed integer
r <- setDatatype(r, datatype="integer", datasize=2)
# 2 byte unsigned integer
r <- setDatatype(r, datatype="integer", datasize=2, signed=FALSE)
# very large integers
r <- setDatatype(r, datatype="numeric", datasize=8)
# single byte values
r <- setDatatype(r, datatype="integer", datasize=1, signed=FALSE)
}

\keyword{ spatial }

