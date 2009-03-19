# R function for the raster package
# Author: Robert J. Hijmans
# International Rice Research Institute. Philippines
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.8
# Licence GPL v3


alignBbox <- function(bndbox, object) {
	oldbb <- extent(object)
	bndbox@xmin <- max(bndbox@xmin, oldbb@xmin)
	bndbox@xmax <- min(bndbox@xmax, oldbb@xmax)
	bndbox@ymin <- max(bndbox@ymin, oldbb@ymin)
	bndbox@ymax <- min(bndbox@ymax, oldbb@ymax)
	col <- colFromX(object, bndbox@xmin)
	mn <- xFromCol(object, col) - 0.5 * xres(object)
	mx <- xFromCol(object, col) + 0.5 * xres(object)
	if (abs(bndbox@xmin - mn) > abs(bndbox@xmin - mx)) { bndbox@xmin <- mx } else { bndbox@xmin <- mn }
	col <- colFromX(object, bndbox@xmax)
	mn <- xFromCol(object, col) - 0.5 * xres(object)
	mx <- xFromCol(object, col) + 0.5 * xres(object)
	if (abs(bndbox@xmax - mn) > abs(bndbox@xmax - mx)) { bndbox@xmax <- mx } else { bndbox@xmax <- mn }
	row <- rowFromY(object, bndbox@ymin)
	mn <- yFromRow(object, row) - 0.5 * yres(object)
	mx <- yFromRow(object, row) + 0.5 * yres(object)
	if (abs(bndbox@ymin - mn) > abs(bndbox@ymin - mx)) { bndbox@ymin <- mx } else { bndbox@ymin <- mn }
	row <- rowFromY(object, bndbox@ymax)
	mn <- yFromRow(object, row) - 0.5 * yres(object)
	mx <- yFromRow(object, row) + 0.5 * yres(object)
	if (abs(bndbox@ymax - mn) > abs(bndbox@ymax - mx)) { bndbox@ymax <- mx } else { bndbox@ymax <- mn }
	return(bndbox)
}


