# Author: Angelo Carlo D. PAcheco and Jorrel  Khalil Aunario
# Date : September 9, 2009
# Version 0.1
# Licence GPL v3


library(raster)

# Function for converting TRMM 3B43 v6 ascii data to raster 
#  Indicate the number of header lines to remove (except column names)
asciiToRaster<- function(filename, outputFName="trmm.tif", hdrs=5){


	write.table(readLines(filename)[-(1:hdrs)], "temp.txt", row.names=F, col.names=F, quote=F)
	coor <- read.table("temp.txt", header=T)

	y = max(coor[,1]) - min(coor[,1]) 
	y = y*4

	x = max(coor[,2]) - min(coor[,2]) 
	x = x*4

	rast <- raster(nrow=y, ncol=x, xmn=min(coor[,2]), xmx=max(coor[,2]), ymn=min(coor[,1]), ymx=max(coor[,1]) )
	rast[] <- -9999

	vec <- 1:length(rast[])
	vec[] <- -9999

	c <- cellFromXY(rast, cbind(coor[,2],coor[,1]) )
	vec[c] <- coor[,3]

	rast <- setValues(rast, vec)

	filename(rast) <- outputFName
	writeRaster(rast, file="GTiff")

}