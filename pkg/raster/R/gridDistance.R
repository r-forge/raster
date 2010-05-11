# Author: Jacob van Etten
# email jacobvanetten@yahoo.com
# Date :  May 2010
# Version 1.0.x
# Licence GPL v3

#the igraph method is speedy for a low number of origin cells, as it calculates shortest distances for each origin cell individually. 
#We could ask the igraph author to make it possible to calculate shortest distance for several origin nodes simultaneously. 
#However, I think I asked this more than a year ago and he seemed not very convinced.

#setGeneric("gridDistance", function(object, ...) standardGeneric("gridDistance"))

#setMethod("gridDistance", signature(object = "RasterLayer"), def =	

gridDistance <- function(object, originValue, omitValue, filename="", ...) 
{
	if(require(igraph))
	{
		if ((dataContent(object) != 'all') & (dataSource(object) != 'disk')) 
		{
			stop('cannot compute distance on a RasterLayer with no data')
		}
		lonlat <- .couldBeLonLat(object)
		filename <- trim(filename)
	
		if(canProcessInMemory(object, n=5) & !(.toDisk())) 
		{
			vals <- getValues(object)
			outRaster <- raster(object)
			oC <- which(vals %in% originValue) #select cells not surrounded by other origin cells
			ftC <- which(!(vals %in% omitValue))
			chunkSize <- ncell(object)
			outRaster[] <- .calcDist(object, chunkSize, ftC, oC)
		} 
		else 
		{
			f1 <- rasterTmpFile()
			r1 <- writeStart(raster(object), filename=f1, overwrite=TRUE)
			hChunks <- min(5, nrow(object))
			nChunks <- ceiling(nrow(object) / hChunks)
			hChunks <- rep(hChunks, nChunks)
			hChunks[nChunks] <- nrow(object) - (nChunks - 1) * hChunks[1]
			pb <- pbCreate(nChunks*2-1, type=.progress(...))
			
			#going down
			for(iChunk in 1:nChunks)
			{
				chunk <- getValues(object, (iChunk - 1)* hChunks[1] + 1, hChunks[iChunk])
				chunkSize <- length(chunk)
				startCell <- (iChunk -1) * hChunks[1] * ncol(object)
				oC <- which(chunk %in% originValue) 
				ftC <- which(!(chunk %in% omitValue))

				chunkDist <- .calcDist(object, chunkSize, ftC, oC)
				if(iChunk>1)
				{
					
					chunkDist <- pmin(chunkDist,
						.calcDist(object, 
							chunkSize=chunkSize+ncol(object), 
							ftC=c(lastRowftC, ftC+ncol(object)), 
							oC = c(lastRowftC, oC+ncol(object)), 
							perCell=c(lastRowDist, rep(0,times=length(oC))), 
							startCell = startCell - ncol(object))[-(1:length(lastRowftC))])
				}
				writeValues(r1, chunkDist, (iChunk - 1) * hChunks[1] + 1)
				lastRow <- chunk[(length(chunk)-ncol(object)+1):length(chunk)]
				lastRowDist <- chunkDist[(length(chunkDist)-ncol(object)+1):length(chunkDist)]
				lastRowftC <- which(!(lastRow %in% omitValue))
				pbStep(pb, iChunk) 
			}
			r1 <- writeStop(r1)

			#then up again
			if(nChunks>1)
			{
				firstRow <- chunk[1:nrow(object)]
				firstRowDist <- chunkDist[1:nrow(object)]
				fileFormat <- .filetype() #to restore the options later
				setOptions(format="GTiff") #in order not to create a .grd in next line -- filling from bottom not possible
				f2 <- rasterTmpFile()
				setOptions(fileFormat) #restore to original value
				r2 <- writeStart(raster(r1), f2, overwrite=TRUE)
				
				writeValues(r2, chunkDist, start = (nChunks - 1) * hChunks[1] +1)

				for(iChunk in (nChunks-1):1)
				{
					startCell <- (iChunk - 1) * hChunks[1] * ncol(object)
					chunk <- getValues(object, (iChunk - 1)* hChunks[1] + 1, hChunks[iChunk])
					chunkSize <- length(chunk)
					oC <- which(chunk %in% originValue) 
					ftC <- which(!(chunk %in% omitValue))
					chunkDist <- getValues(r1, (iChunk - 1)* nChunks + 1, hChunks[iChunk])
					firstRowftC <- which(!(firstRow %in% omitValue)) + chunkSize
					chunkDist <- pmin(chunkDist,
						.calcDist(object, 
							chunkSize=chunkSize+ncol(object), 
							ftC=c(ftC, firstRowftC), 
							oC=c(oC, firstRowftC), 
							perCell=c(rep(0,times=length(oC)),firstRowDist), 
							startCell=startCell)[1:chunkSize])
					writeValues(r2, chunkDist, start = (iChunk - 1) * hChunks + 1)
					firstRow <- chunk[1:nrow(object)]
					firstRowDist <- chunkDist[1:nrow(object)]
					pbStep(pb, 2*nChunks - iChunk)
				}
				r2 <- writeStop(r2)
				pbClose(pb)
				outRaster <- r2
				#why not clean up the mess?
				#rm(r2)
				#unlink(f2)
			}
			else{outRaster <- r1}
			#why not clean up the mess?
			#rm(r1)
			#unlink(f1)
			if(filename == "")
			{
				outRaster <- readAll(outRaster)
			}
		}
	}
	if (filename != "") 
	{
		outRaster <- writeRaster(outRaster, filename=filename, ...)
	}
	return(outRaster)
}

.calcDist <- function(object, chunkSize, ftC, oC, perCell=0, startCell=0)
{
	shortestPaths <- rep(Inf, times=max(ftC))
	if(length(oC)>0)
	{
		lonlat <- .couldBeLonLat(object)
		adj <- adjacency(object,fromCells=ftC,toCells=ftC,directions=8) #OPTIMIZE: omit oC cells surrounded by other origin cells
		distGraph <- graph.edgelist(adj-1, directed=FALSE)
		if (lonlat) 
		{
			coord <- cbind(xyFromCell(object,adj[,1]+startCell),xyFromCell(object,adj[,2]+startCell))
			distance <- apply(coord,1,function(x){pointDistance(x[1:2],x[3:4], type='GreatCircle')})
			E(distGraph)$weight <- distance
		}
		else
		{
			E(distGraph)$weight <- 1 #Pythagoras using xres and yres, perhaps using adjacency() here instead of above
		}
		shortestPaths <- pmin(shortestPaths, apply(shortest.paths(distGraph, oC-1) + perCell, 2, min))
		if(max(ftC) < chunkSize){shortestPaths <- c(shortestPaths,rep(Inf,times=chunkSize-max(ftC)))}
	}
	return(shortestPaths)
}
