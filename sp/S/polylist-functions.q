# Polygon functions

Map2Poly4 <- function(Map, region.id=NULL, projargs=as.character(NA), raw=FALSE) {
	if (class(Map) != "Map") stop("not a Map")
	if (attr(Map$Shapes,'shp.type') != 'poly')
		stop("maptype not poly")
	Map2maplim <- function(Map) {
		if (class(Map) != "Map") stop("not a Map")
		res <- cbind(attr(Map$Shapes, 'minbb')[1:2], 
			attr(Map$Shapes, 'maxbb')[1:2])
#ifdef R
		rownames(res) <- c("x", "y")
		colnames(res) <- c("min", "max")
#else
		dimnames(res) <- list(c("x", "y"), c("min", "max"))
#endif
		res
	}
	if (is.null(region.id) || 
		length(region.id) != attr(Map$Shapes,'nshps')) {
		region.id <- as.character(1:attr(Map$Shapes,'nshps'))
	} else {
		region.id <- as.character(region.id)
	}
	res <- .get.Polylist4(Map=Map, region.id=region.id, projargs=projargs, raw=raw)

	pO <- as.integer(1:attr(Map$Shapes,'nshps'))
	after <- as.integer(rep(NA, attr(Map$Shapes,'nshps')))
	r1 <- .insiders(res)
	if (!all(sapply(r1, is.null))) {
		after <- as.integer(sapply(r1, function(x) ifelse(is.null(x), NA, max(x))))
		pO <- order(after, na.last=FALSE)
	}
	if (!raw) {
		rD <- sapply(res, function(x) x@ringDir[which(x@plotOrder == 1)])
		if (any((rD == -1) & is.na(after))) {
			oddCC <- which((rD == -1) & is.na(after))
			for (i in oddCC) {
				tgt <- which(res[[i]]@plotOrder == 1)
				nParts <- res[[i]]@nParts
				tmp <- as.matrix(res[[i]]@coords)
				from <- res[[i]]@pStart.from[tgt]
				to <- res[[i]]@pStart.to[tgt]
				tmp[from:to,] <- res[[i]]@coords[to:from, ]
# 				attributes(tmp) <- attributes(res[[i]])
				tmp1 <- new("Polygon4", bbox=res[[i]]@bbox,
				proj4string=res[[i]]@proj4string,
				coords=tmp, nVerts=res[[i]]@nVerts,
				nParts=res[[i]]@nParts,
				pStart.from=res[[i]]@pStart.from,
				pStart.to=res[[i]]@pStart.to,
				RingDir=res[[i]]@RingDir,
				ringDir=as.integer(NA),
				region.id=res[[i]]@region.id,
				plotOrder=res[[i]]@plotOrder,
				after = res[[i]]@after)

				rD <- vector(length=nParts, mode="integer")
				for (j in 1:nParts) rD[j] <- .ringDir4(tmp1, j)
				tmp1@ringDir <- as.integer(rD)
				res[[i]] <- tmp1
				warning(paste("ring direction changed in polygon", i))
			}
		}
	}
	if (raw) warning("holes handled as in original data - check not performed")	
	SD <- new("SpatialData", bbox=Map2maplim(Map), 
		proj4string=CRS(projargs))
	PL4 <- new("Polylist4", SD, polygons=res, region.id=region.id, 
		plotOrder=as.integer(pO), after=as.integer(after))
	PL4
}

.get.Polylist4 <- function(Map, region.id=NULL, projargs=NA, raw=FALSE) {
	n <- attr(Map$Shapes,'nshps')
	CRSobj <- CRS(projargs)
	res <- vector(mode="list", length=n)
	nParts <- integer(n)
	for (i in 1:n) nParts[i] <- attr(Map$Shapes[[i]], "nParts")
	for (i in 1:n) {
		bbox <- matrix(c(attr(Map$Shapes[[i]], "bbox")), 2, 2)
#ifdef R
		rownames(bbox) <- c("x", "y")
		colnames(bbox) <- c("min", "max")
#else
		dimnames(bbox) <- list(c("x", "y"), c("min", "max"))
#endif
		SD <- new("SpatialData", bbox=bbox, proj4string=CRSobj)
		if (nParts[i] > 1)
			res[[i]] <- .getMultiShp4(Map$Shapes[[i]], 
			nParts[i], SD, region.id[i], raw=raw)
		else {	
			res[[i]] <- new("Polygon4", SD,
				coords=Map$Shapes[[i]]$verts,
				nVerts=as.integer(attr(Map$Shapes[[i]], 
					"nVerts")),
				nParts=as.integer(1),
				pStart.from=as.integer(1),
				pStart.to=as.integer(attr(Map$Shapes[[i]], 
					"nVerts")),
				RingDir=as.integer(as.vector(attr(
					Map$Shapes[[i]], "RingDir"))),
				ringDir=as.integer(NA),
				region.id=region.id[i],
				plotOrder=as.integer(1),
				after = as.integer(1))
			res[[i]]@ringDir <- as.integer(.ringDir4(res[[i]], 1))
		}
	}
	invisible(res)
}

.getMultiShp4 <- function(shp, nParts, SD, reg.id, raw=FALSE) {
	Pstart <- shp$Pstart
	nVerts <- attr(shp, "nVerts")
	from <- integer(nParts)
	to <- integer(nParts)
	from[1] <- 1
	for (j in 1:nParts) {
		if (j == nParts) to[j] <- nVerts
		else {
			to[j] <- Pstart[j+1]
			from[j+1] <- to[j]+1
		}
	}
	res <- shp$verts[from[1]:to[1],]
	if (nParts > 1) {
	    for (j in 2:nParts) {
		res <- rbind(res, c(NA, NA))
		res <- rbind(res, shp$verts[from[j]:to[j],])
	     }
	}
	for (j in 1:nParts) {
		from[j] <- from[j] + (j-1)
		to[j] <- to[j] + (j-1)
	}
	pO <- as.integer(1:nParts)
	after <- as.integer(rep(NA, nParts))
	res1 <- vector(mode="list", length=nParts)
	for (i in 1:nParts) res1[[i]] <- res[from[i]:to[i],]
	r1 <- .insiders(res1)
	if (!all(sapply(r1, is.null))) {
		after <- as.integer(sapply(r1, 
			function(x) ifelse(is.null(x), NA, max(x))))
		pO <- order(after, na.last=FALSE)
	}
	P4 <- new("Polygon4", SD,
		coords=res,
		nVerts=as.integer(nVerts),
		nParts=as.integer(nParts),
		pStart.from=as.integer(from),
		pStart.to=as.integer(to),
		RingDir=as.integer(as.vector(attr(shp, "RingDir"))),
		ringDir=as.integer(NA),
		region.id=reg.id,
		plotOrder=as.integer(pO),
		after = as.integer(after))
	rD <- vector(length=nParts, mode="integer")
	for (j in 1:nParts) rD[j] <- .ringDir4(P4, j)
	P4@ringDir <- as.integer(rD)
	if (!raw) {
		top <- which(pO == 1)
		if (any((rD[-top] == -1) & is.na(after[-top]))) {
			oddCC <- which((rD == -1) & is.na(after))
			for (i in oddCC) {
				if (i != top) {
					from1 <- from[i]
					to1 <- to[i]
					P4@coords[from[i]:to[i],] <- 
						P4@coords[to[i]:from[i],]
					P4@ringDir[i] <- .ringDir4(P4, i)
					warning(paste("ring direction changed in subpolygon"))
				}
			}
		}


	}
	invisible(P4)
}

.insiders <- function(pl) {
	bbox1 <- function(x) {
		if (is.matrix(x)) {
			r1 <- range(x[,1], na.rm=TRUE)
			r2 <- range(x[,2], na.rm=TRUE)
		} else if (is(x, "Polygon4")) {
			r1 <- range(x@coords[,1], na.rm=TRUE)
			r2 <- range(x@coords[,2], na.rm=TRUE)
		} else {
			stop("unknown polylist")
		}
		res <- c(r1[1], r2[1], r1[2], r2[2])
		res
	}

	n <- length(pl)
	bbs <- matrix(0, nrow=n, ncol=4)
	for (i in 1:n) bbs[i,] <- bbox1(pl[[i]])
	res <- .Call("insiders", as.integer(n), as.double(bbs), PACKAGE="sp")
	res
}

# based on SHPRingDir_2d, modified to use current ring only, and to strip
# out last vertex if identical with first

.ringDir4 <- function(xy, ring) {
	nParts <- xy@nParts
	if (ring > nParts) stop("ring too large")
	from <- xy@pStart.from
	to <- xy@pStart.to
	a <- xy@coords[from[ring]:to[ring],1]
	b <- xy@coords[from[ring]:to[ring],2]
	nvx <- length(b)

	if((a[1] == a[nvx]) && (b[1] == b[nvx])) {
		a <- a[-nvx]
		b <- b[-nvx]
		nvx <- nvx - 1
	}

	tX <- 0.0
	dfYMax <- max(b)
	ti <- 1
	for (i in 1:nvx) {
		if (b[i] == dfYMax && a[i] > tX) ti <- i
	}
	if ( (ti > 1) & (ti < nvx) ) { 
		dx0 = a[ti-1] - a[ti]
      		dx1 = a[ti+1] - a[ti]
      		dy0 = b[ti-1] - b[ti]
      		dy1 = b[ti+1] - b[ti]
   	} else if (ti == nvx) {
		dx0 = a[ti-1] - a[ti]
      		dx1 = a[1] - a[ti]
      		dy0 = b[ti-1] - b[ti]
      		dy1 = b[1] - b[ti]
   	} else {
#   /* if the tested vertex is at the origin then continue from 0 (1) */ 
     		dx1 = a[2] - a[1]
      		dx0 = a[nvx] - a[1]
      		dy1 = b[2] - b[1]
      		dy0 = b[nvx] - b[1]
   	}
	v3 = ( (dx0 * dy1) - (dx1 * dy0) )
	if ( v3 > 0 ) return(as.integer(1))
   	else return(as.integer(-1))
}

# .polygon tries to catch the numerous R/S-Plus differences...
.polygon = function(x, y = NULL, density = NULL, angle = 45,
	border = NULL, col = NA, lty = NULL, xpd = NULL, ...) {
#ifdef R
	polygon(x = x, y = y, density = density, angle = angle,
		border = border, col = col, lty = lty, xpd = xpd, ...)
#else
	# polygon(x, y, density=-1, angle=45, border=T, col=par("col"))
	if (is.matrix(x))
		dimnames(x) = list(NULL, c("x", "y")) # may not be necessary
	if (is.null(density))
		density = -1
	if (!(is.logical(border) && !is.na(border)))
		border = is.null(border)
	else if (is.na(border))
		border = F
	if (is.na(col))
		col = par("col")
	polygon(x = x, density = density, angle = angle,
		border = border, col = col, ...)
#endif
}

plot.Polylist4 <- function(x, col, border = NULL, add=FALSE, xlim=NULL, 
	ylim=NULL, xlab="", ylab="", asp=1, xpd = NULL, density = NULL, 
	angle = 45, pbg, ...) {

	if (missing(pbg))
#ifdef R
		pbg = par("bg") # transparent!
#else
		pbg = 0
#endif

	if (!is(x, "Polylist4")) 
		stop("Not a Polygon4 object")
	usrpoly <- function(x) {
		p <- matrix(c(x[1], x[2], x[2], x[1], x[3], x[3], 
			x[4], x[4]), ncol=2)
		p
	}
	if (!add) {
		maplim <- x@bbox
		if (is.null(maplim))
			if (is.null(xlim) || is.null(ylim))
				stop("map limits missing")
		if (is.null(xlim)) xlim <- c(maplim["x",])
		if (is.null(ylim)) ylim <- c(maplim["y",])
		plot(x=xlim, y=ylim, xlim=xlim, ylim=ylim, type="n",
		asp=asp, xlab=xlab, ylab=ylab, ...)
		.polygon(usrpoly(par("usr")), col = pbg, border = NA)
	}
	if (missing(col)) col <- NA
	n <- length(x@polygons)
	if (length(border) != n)
		border <- rep(border, n, n)
    if (!is.null(density)) {
		if (length(density) != n)
			density <- rep(density, n, n)
		if (length(angle) != n)
			angle <- rep(angle, n, n)
		for (j in x@plotOrder) 
				.polygonHolesh(x@polygons[[j]], border = border[j], 
				xpd = xpd, density = density[j], angle = angle[j], pbg = pbg)
	} else {
		if (length(col) != n)
			col <- rep(col, n, n)
		for (j in x@plotOrder) 
			.polygonHolesh(x@polygons[[j]], col=col[j], 
				border=border[j], xpd = xpd, pbg = pbg)
	}
}

.polygonHolesh <- function(P4, col=NA, border=NULL, xpd=NULL, density=NULL,
	angle=45, pbg) {
	coords <- P4@coords
	nParts <- P4@nParts
	pFrom <- P4@pStart.from
	pTo <- P4@pStart.to
	if (is.na(col)) hatch <- TRUE
	else hatch <- FALSE
	for (i in P4@plotOrder) {
		if (hatch) {
			if (P4@ringDir[i] == 1)
				.polygon(coords[pFrom[i]:pTo[i],], 
					border = border, xpd = xpd, 
					density = density, angle = angle)
			else .polygon(coords[pFrom[i]:pTo[i],], 
					border = border, xpd = xpd, col=pbg, 
					density = NULL)
		} else {
			if (P4@ringDir[i] == 1)
				.polygon(coords[pFrom[i]:pTo[i],], 
					border = border, xpd = xpd, col=col)
			else .polygon(coords[pFrom[i]:pTo[i],], 
				border = border, xpd = xpd, col=pbg)
		}
	}
}



.map2polys = function(map) {
	get.polygon = function(x, map) {
		range = x[1]:x[2]
		c(map$x[range], map$y[range])
	}
	fold.polygon = function(x) {
		n = length(x)
		half = n/2
		cbind(xcoord = x[1:half], ycoord = x[(half+1):n])
	}
	if (!inherits(map, "map"))
		stop("map expected")
	xc = map$x
	n = length(xc)
	breaks = which(is.na(xc))
	range.limits = cbind(start = c(1, breaks + 1), end = c(breaks - 1, n))
	lst = apply(range.limits, 1, get.polygon, map = map)
	names(lst) = map$names
	lapply(lst, fold.polygon)
}

map2Poly4 = function(map) {
	poly2Poly4 = function(pol) {
		bb = t(apply(pol, 2, range))
		dimnames(bb) = list(c("x", "y"), c("min", "max"))
		nverts = dim(pol)[1]
		new("Polygon4",
			bbox = bb,
			proj4string = CRS(as.character(NA)),
			coords = pol,
			nVerts = nverts,
			nParts = as.integer(1),
			pStart.from = as.integer(1),
			pStart.to = as.integer(nverts),
			RingDir = as.integer(-1),
			ringDir = as.integer(NA),		
			region.id = "xx",
			plotOrder=as.integer(1),
			after = as.integer(1))
	}
	polys = .map2polys(map)
	polys0 = lapply(polys, poly2Poly4)
	for (i in 1:length(polys0)) 
		polys0[[i]]@ringDir <- as.integer(.ringDir4(polys0[[i]], 1))

	# set up bb
	getBBx = function(x) range(x@bbox[1,])
	getBBy = function(x) range(x@bbox[2,])
	rx = sapply(polys0, getBBx)
	ry = sapply(polys0, getBBy)
	bb = t(cbind(c(min(rx[1,]), max(rx[2,])), c(min(ry[1,]), max(ry[2,]))))
	# bb = matrix(x$range, 2, 2)
	dimnames(bb) = list(c("x", "y"), c("min", "max"))
	pO <- as.integer(1:length(polys0))
	after <- as.integer(rep(NA, length(polys0)))
	r1 <- .insiders(polys0)
	if (!all(sapply(r1, is.null))) {
		after <- as.integer(sapply(r1, 
			function(x) ifelse(is.null(x), NA, max(x))))
		pO <- order(after, na.last=FALSE)
	}
	new("Polylist4",
		bbox = bb,
		proj4string = CRS(as.character(NA)),
		polygons = polys0,
		region.id = names(polys),
		plotOrder=as.integer(pO),
		after = as.integer(after))
}

gpcPoly2P4 <- function(x) {
	poly2Poly4 = function(pol) {
		bb = t(apply(pol, 2, range))
		dimnames(bb) = list(c("x", "y"), c("min", "max"))
		nverts = dim(pol)[1]
		new("Polygon4",
			bbox = bb,
			proj4string = CRS(as.character(NA)),
			coords = pol,
			nVerts = nverts,
			nParts = as.integer(1),
			pStart.from = as.integer(1),
			pStart.to = as.integer(nverts),
			RingDir = as.integer(-1),
			ringDir = as.integer(NA),		
			region.id = "xx",
			plotOrder=as.integer(1),
			after = as.integer(1))
	}
#	if (!is(x, "gpc.poly")) stop("not a gpc.poly object")
	pl <- lapply(attr(x, "pts"), function(x) cbind(as.double(x$x), 
		as.double(x$y)))
	polys = pl
	polys0 = lapply(polys, poly2Poly4)
	for (i in 1:length(polys0)) 
		polys0[[i]]@ringDir <- as.integer(.ringDir4(polys0[[i]], 1))

	# set up bb
	getBBx = function(x) range(x@bbox[1,])
	getBBy = function(x) range(x@bbox[2,])
	rx = sapply(polys0, getBBx)
	ry = sapply(polys0, getBBy)
	bb = t(cbind(c(min(rx[1,]), max(rx[2,])), c(min(ry[1,]), max(ry[2,]))))
	# bb = matrix(x$range, 2, 2)
	dimnames(bb) = list(c("x", "y"), c("min", "max"))
	pO <- as.integer(1:length(polys0))
	after <- as.integer(rep(NA, length(polys0)))
	r1 <- .insiders(polys0)
	if (!all(sapply(r1, is.null))) {
		after <- as.integer(sapply(r1, 
			function(x) ifelse(is.null(x), NA, max(x))))
		pO <- order(after, na.last=FALSE)
	}
	new("Polylist4",
		bbox = bb,
		proj4string = CRS(as.character(NA)),
		polygons = polys0,
		region.id = as.character(1:length(polys0)),
		plotOrder=as.integer(pO),
		after = as.integer(after))

}
