\name{overlay}

\docType{methods}

\alias{overlay}
\alias{overlay,RasterLayer,RasterLayer-method}
\alias{overlay,RasterLayer,missing-method}
\alias{overlay,RasterStack,missing-method}
\alias{overlay,list,missing-method}

\title{overlay}

\description{
Calculate values for a new RasterLayer object, based on two or more existing and RasterLayers of the same extent and resolution. 
overlay is a generic functon used in the sp and raster packages. This help file describes its use in the raster package. 

You should supply a function \code{fun} to set the way that the RasterLayers are combined. The number of arguments in the function must match the number of RasterLayer objects.
For example, if you merge two RasterLayers you could use multiply: \code{fun=function(x,y){return(x*y)}} percentage: \code{fun=function(x,y){return(100 * x / y)}}. 
The default function \code{sum} always works because its first argument is \code{...}
}

\section{Methods}{
\describe{
Formal definition of the overlay method (in the sp package)

\code{overlay(x, y, ...)}

\bold{1) x and y are Raster* objects}

\code{overlay(x, y, ..., fun=sum, overwrite=FALSE, filetype='raster', datatype='FLT4S', track=-1)}

\tabular{rll}{
\tab \code{x} \tab a RasterLayer object \cr
\tab \code{y} \tab a RasterLayer object \cr
\tab \code{...} \tab additional RasterLayer objects \cr
\tab \code{fun} \tab the function to be applied. This should be a function that takes two numbers as an argument  \cr
\tab \code{filename} \tab  filename for the output raster. A valid filename must be provided when the data of the input rasters are on disk \cr
\tab \code{overwrite}\tab logical. If \code{TRUE}, existing files will be overwritten \cr
\tab \code{filetype} \tab output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeRaster}} \cr
}
  
\bold{2) x is a Raster* object, y is missing (this is equivalent to \code{\link[raster]{calc}})}

\code{overlay(x, fun=sum, overwrite=FALSE, filetype='raster', datatype='FLT4S', track=-1)}

\tabular{rll}{
  \tab \code{x}  \tab a RasterLayer object \cr
  \tab \code{fun}  \tab the function to be appliepd. This should be a function that takes two numbers as an argument \cr
  \tab \code{filename}  \tab  filename for the output raster. A valid filename must be provided when the data of the input rasters are on disk \cr
  \tab \code{overwrite}  \tab logical. If \code{TRUE}, existing files will be overwritten \cr
  \tab \code{filetype}  \tab output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeRaster}} \cr
}
  

}
}

\details{
In stead of the overlay function you can also use normal artimic functions such as *, /, +, - with RasterLayer objects (see examples), 
but then you cannot specifiy an output filename, filetype or datatype. Overlay should be more efficient when using large data files that 
cannot be loaded into memory.
}

\value{
a new RasterLayer object and in some cases, if a filename is provided or the output values cannot be stored in memory, 
the side effect of a file on disk
}

\seealso{ \code{\link[raster]{Arith-methods}} }

\author{
Robert J. Hijmans 
}

\examples{
r <- raster(ncol=10, nrow=10)
r1 <- init(r)
r2 <- init(r)
r3 <- overlay(r1, r2, fun=function(x,y){return(x+y)})

# long version for multiplication
r4 <- overlay(r1, r2, fun=function(x,y){return(x*y)} )

#use a stack
s <- stack(r1, r2)
r5 <- overlay(s, fun=function(x,y){return(x*y)} )

# use a single RasterLayer (same as calc function)
r6 <- overlay(r1, fun=function(x){return(sqrt(x))} )

# multiplication with more than two layers (make sure the number of RasterLayers matches the arguments of 'fun'
r7 <- overlay(r1, r2, r3, r4, fun=function(a,b,c,d){return(a*b+c*d)} )  
# equivalent function, efficient if values can be loaded in memory
r8 <- r1 * r2 + r3 * r4

}
 
\keyword{methods}
\keyword{spatial}
