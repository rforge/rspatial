"bubble" <- function (obj, zcol = 1, fill = TRUE, maxsize = 3, do.sqrt = TRUE, 
	pch, col = c(2, 3), key.entries = quantile(data[,zcol]),
	main = ifelse(is.numeric(zcol), names(data)[zcol], zcol),
    identify = FALSE, labels = row.names(data.frame(obj)), ...) 
{
	if (!extends(class(obj), "SpatialDataFrame"))
		stop("first object is not of (or does not extend) class SpatialDataFrame")
	data = obj@data[-obj@coord.columns]
	cc = coordinates(obj)
    x = cc[, 1]
    y = cc[, 2]
	if (NCOL(data) == 1)
		z = data
	else if (NCOL(data) == 0)
		z = rep(1, NROW(cc))
	else
    	z = data[, zcol]
    if (missing(pch)) 
        pch = ifelse(fill, 16, 1)
    z.col = as.vector(ifelse(z < 0, col[1], col[2]))
    q = key.entries
    q.pch = rep(pch, length(q))
    q.text = as.character(round(q, 3))
    q.col = as.vector(ifelse(q < 0, col[1], col[2]))
    az = abs(z)
    q = abs(q)
    if (do.sqrt) {
		az = sqrt(az)
		q = sqrt(q)
    }
    cex = as.vector(maxsize * az/max(az))
    q.cex = as.vector(maxsize * q/max(az))

    if (identify) {
		plot(x, y, asp = 1, cex = cex, main = main, ...)
		return(identify(x, y, labels))
	} else {
#ifdef R
		require(lattice)
#endif
    	key = list(space = "right", points = list(pch = q.pch, col = q.col, 
    		cex = q.cex), text = list(q.text))
		xyplot(y ~ x, col = z.col, cex = cex, pch = pch, asp = mapasp(obj), 
        	key = key, main = main, ...)
	}
}