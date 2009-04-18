

rasterToAccess <- function(raster, filename) {
	ext(filename) <- '.mdb'
	if (!file.exists(filename)) {
		empty <- system.file("db/template", package="raster")
		file.copy(empty, filename)
	}
	accdb <- odbcConnectAccess(filename)
	ind <- sqlQuery(accdb, "SELECT * FROM [index]")
	ln1 <- layerNames(raster)
	ln1 <- substr(ln1, 1, 60)
	if (ln1 == "") {
		ln1 <- deparse(substitute(raster))
	}
	ln <- ln1
	cnt <- 2
	while (ln %in% ind$tablename) {
		ln <- paste(ln1, "_", cnt, sep="")
		cnt <- cnt + 1
	}

	df <- data.frame(t(c(ln, 1, nrow(raster), ncol(raster), xmin(raster), xmax(raster), ymin(raster), ymax(raster))))
	colnames(df) <- c('tablename', 'nlayers', 'nrow', 'ncol', 'xmin', 'xmax', 'ymin', 'ymax')
	sqlSave(accdb, df, tablename = 'index', append=TRUE, fast=FALSE, rownames=FALSE, safer=FALSE)
	
	filename(raster) <- paste(filename, '#', ln, sep="")
	
	if (dataContent(raster) == 'all') {
		df <- as.data.frame(values(raster))
		colnames(df) <- 'values'
		sqlSave(accdb, df, tablename = ln, append=FALSE, fast = FALSE, rownames=FALSE, safer=FALSE) 
	}
	
	odbcClose(accdb)
	return(raster)
}
