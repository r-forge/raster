\name{SetDatatype}
\alias{datatype<-}
\alias{setDatatype}

\title{ Set data type of a RasterLayer}

\description{
Use these functions to set the datatype properties of a RasterLayer object. The datatype determines how values are written to disk. 
}

\usage{
setDatatype(raster, datatype)
}

\arguments{
  \item{raster}{ A \code{RasterLayer} object }
  \item{datatype}{ the type of data for writing values to disk. See below }
}

\details{

Setting the data type is useful if you want to write values to disk. In other cases use functions such as round()

While you can access the slots of the object directly, you would do that at your own peril because the value of one raster property can affect values in other properties, and changing slot values may thus break the integrity of the objects. Using slots directly may also lead to code to break in the future if underlying class definitions change. 

Datatypes are described by 5 characters. The first three indicate whether the values are integers, decimal number or logical values. The fourth character indicates the number of bytes used to save the values on disk, and the last character indicates whether the numbers are signed (i.e. negative and positive values) or not (only zero and positive values)

The following datatypes are available.

datatype, size, signed: range of possible values

'LOG1S', FALSE (0) to TRUE (1)

'INT1S: -127 to 127

'INT1U': 0 to 255

'INT2S': -32,767 to 32,767

'INT2U': 0 to 65,534

'INT4S': -2,147,483,647 to 2,147,483,647

'INT4U': 0 to 4,294,967,294

'INT8S': -9,223,372,036,854,775,807 to 9,223,372,036,854,775,807

'INT4U': 0 to 18,446,744,073,709,551,614

'FLT4S': -3.4E38 to 3.4E38

'FLT8S':  -1.7E308 to  1.7E308


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
r <- setDatatype(r, "INT2S")
# this is equivalent to 
datatype(r) <- "INT2S"
# 2 byte unsigned integer
r <- setDatatype(r, "INT2U")
# very large integers
r <- setDatatype(r, "INT8S")
# single byte values (0-255)
r <- setDatatype(r, "INT1U")
#double precision decimal numbers
r <- setDatatype(r, "FLT8S")
}

\keyword{ spatial }

