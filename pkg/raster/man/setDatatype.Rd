\name{setDatatype}
\alias{datatype<-}
\alias{setDatatype}

\title{ Set data type }

\description{
Use these functions to set the datatype properties of a RasterLayer object. The datatype determines how values are written to disk. 
It does not affect the way they are stored in memory.
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

\tabular{rll}{
\bold{datatype \tab min \tab max} \cr
LOG1S \tab  FALSE (0)\tab TRUE (1) \cr
INT1S \tab -127 \tab  127\cr
INT1U \tab 0 \tab  255\cr
INT2S \tab -32,767\tab  32,767\cr
INT2U \tab  0 \tab  65,534\cr
INT4S \tab  -2,147,483,647 \tab  2,147,483,647\cr
INT4U \tab  0 \tab  4,294,967,294\cr
INT8S \tab  -9,223,372,036,854,775,807 \tab  9,223,372,036,854,775,807\cr
INT4U \tab  0 \tab  18,446,744,073,709,551,614\cr
FLT4S \tab  -3.4E38 \tab  3.4E38\cr
FLT8S \tab   -1.7E308 \tab   1.7E308\cr
}

For all integer types, except the single byte types, the lowest (signed) or highest (unsigned) value is used to store NA. 
Single byte files do not have NA values.
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

