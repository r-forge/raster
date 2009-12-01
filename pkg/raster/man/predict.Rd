\name{predict}

\docType{methods}

\alias{predict}
\alias{predict,Raster-method}

\title{Model predictions}

\description{
Make a RasterLayer with a prediction based on a a fitted model object. Provide a Raster* with the independent variables. The layerNames in the RasterStack should exactely match those expected by the model. This will be the case if the RasterStack was used (via xyValues) to obtain the values to fit the model (see the example). Any type of model (e.g. glm. gam, randomforest) for which a predict method has been implemented can be used. If you have 'x' and 'y' as independent variables (i.e. the location of grid cells, xy=TRUE.
}

\usage{
predict(object, ...) 
}


\arguments{
  \item{object}{a RasterStack, RasterBrick or RasterLayer object}
  \item{...}{Additional arguments. See below, under Methods}  
}

\section{Methods}{
\describe{
 
\code{predict(object, model, filename='', xy=FALSE, ...)}
  
\tabular{rll}{  
  \tab  \code{object} \tab a RasterStack, RasterBrick or RasterLayer object \cr
  \tab  \code{model}  \tab A fitted model\cr
  \tab  \code{filename}  \tab Output filename for a new raster; if NA the result is not written to a file but returned with the RasterLayer object, in the data slot \cr
  \tab  \code{xy}  \tab Logical. If \code{TRUE}, 'x' and 'y' (in lowercase), i.e. the location of each cell, are added to the predictor variables (in object); see \link[raster]{predict} \cr
  \tab  \code{...}  \tab Additional arguments \cr
}


The following additional arguments can be passed, to replace default values
\tabular{rll}{
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type; see \code{\link[raster]{dataType}} \cr
  \tab \code{overwrite} \tab Logical. If TRUE, "filename" will be overwritten if it exists \cr
  \tab \code{progress} \tab Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""  \cr
 }

}}

\value{
a RasterLayer object
}

\author{ Robert J. Hijmans}

\examples{
# A simple model to predict the location of the R in the R-logo using 20 presence points 
# and 50 (random) pseudo-absence points. This type of model is often used to predict species distributions

# create a RasterStack (a set of predictor rasters)
require(rgdal)
logo <- stack(system.file("pictures/Rlogo.jpg", package="rgdal"), bands=-1)
r <- sqrt(raster(logo,1) * raster(logo,2))
logo <- addLayer(logo, r)
layerNames(logo) <- c('band1', 'band2', 'band3', 'interaction')

#get presence and absence points
presence <- matrix(c(48.243420, 48.243420, 47.985820, 52.880230, 49.531423, 46.182616, 54.168232, 69.624263, 83.792291, 85.337894, 74.261072, 83.792291, 95.126713,
 84.565092, 66.275456, 41.803408, 25.832176, 3.936132, 18.876962, 17.331359,7.048974, 13.648543, 26.093446, 28.544714, 39.104026, 44.572240, 51.171810, 56.262906,
 46.269272, 38.161230, 30.618865, 21.945145, 34.390047, 59.656971, 69.839163, 73.233228, 63.239594, 45.892154, 43.252326, 28.356155), ncol=2)
# random absence
absence <- cbind(runif(50)*(xmax(logo)-xmin(logo))+xmin(logo), runif(50)*(ymax(logo)-ymin(logo))+ymin(logo))

#extract values for points from stack
xy <- rbind(cbind(1, presence), cbind(0, absence))
v <- cbind(xy[,1], xyValues(logo, xy[,2:3]))
colnames(v)[1] <- 'presabs'

#build a model, here an example with glm 
model <- glm(formula=presabs~., data=data.frame(v))

#predict to a raster
r <- predict(logo, model, progress='text')

plot(r>0.3)
points(presence, bg='blue', pch=21)
points(absence, bg='red', pch=21)

# also try:
# require(randomForest)
# formula <- as.factor(presabs) ~.
# model <- randomForest(formula, data=data.frame(v))
# r2 <- predict(logo, type='response', model, progress='text')
# note the additional argument "type='response'" that is passed to predict.randomForest

}

\keyword{methods}
\keyword{spatial}
