poly.clipper<-function (name, state, statefips = FALSE, level = c("tract", 
    "blk", "blkgrp"), bb.epsilon = 0.006, sp.object = NULL, proj = NULL) 
{
    poly.clipper.aux <- function(name, state, statefips = FALSE, 
        level = c("tract", "blk", "blkgrp"), bb.epsilon = bb.epsilon, 
        sp.object = NULL, proj = NULL) {

       city <- city(name, state, statefips)
        state <- check.state(state, statefips)
        if (is.null(state)) {
            stop("Not a State! \n")
        }
        require(paste("UScensus2000", level, sep = ""), character.only = TRUE)
 
        if (!is.null(sp.object)) {
            temp.level <- sp.object
        }else {
            x <- paste(state, level, sep = ".")
            data(list = x, envir = parent.frame()) ###Check enviroment
            temp.level <- get(x)
        }
        
        as.Polygons.gpc.poly <- function(x, ID) {
            thisPolys <- lapply(get.pts(x), function(p) {
                Polygon(rbind(as.matrix(cbind(p$x, p$y)), c(p$x[1], 
                  p$y[1])), hole = p$hole)
            })
            Polygons(thisPolys, ID)
        }
        
        temp1 <- bbox(city)
        temp1[1, 1] <- temp1[1, 1] - bb.epsilon
        temp1[1, 2] <- temp1[1, 2] + bb.epsilon
        temp1[2, 1] <- temp1[2, 1] - bb.epsilon
        temp1[2, 2] <- temp1[2, 2] + bb.epsilon
        temp <- matrix(c(temp1[1, 1], temp1[2, 1], temp1[1, 2], 
            temp1[2, 1], temp1[1, 2], temp1[2, 2], temp1[1, 1], 
            temp1[2, 2]), ncol = 2, nrow = 4, byrow = TRUE)
        temp.gcp <- as(temp, "gpc.poly")
        temp.pb <- as.Polygons.gpc.poly(temp.gcp, "temp.gcp1")
        test10 <- SpatialPolygons(list(temp.pb), proj4string = CRS("+proj=longlat +datum=NAD83"))
        temp9 <- overlay(SpatialPoints(coordinates(temp.level), 
            proj4string = CRS("+proj=longlat +datum=NAD83")), 
            test10)
        m <- temp.level[which(is.na(temp9) == FALSE), ]
        plys <- slot(m, "polygons")
        coords <- lapply(plys, function(x) {
            slot(slot(x, "Polygons")[[1]], "coords")
        })
        
        gpc.poly.coords <- sapply(coords, as, "gpc.poly")
        city.gpc <- vector("list", length(city@polygons))
        for (i in 1:length(city.gpc)) {
            city.gpc[[i]] <- as(city@polygons[[i]]@Polygons[[1]]@coords, 
                "gpc.poly")
        }
        city.gpc2 <- city.gpc[[1]]
        if (length(city.gpc) > 1) {
            for (i in 2:length(city.gpc)) {
                city.gpc2 <- append.poly(city.gpc2, city.gpc[[i]])
            }
        }
        int.man <- sapply(gpc.poly.coords, intersect, city.gpc2)
        int.man2 <- list()
        intersect.index <- vector()
        for (i in 1:length(int.man)) {
            if (length(int.man[[i]]@pts) > 0) {
                int.man2 <- c(int.man2, list(int.man[[i]]))
                intersect.index <- c(intersect.index, i)
            }else {
            }
        }
        city.blk <- vector("list", length(int.man2))
        for (i in 1:length(int.man2)) {
            city.blk[[i]] <- as.Polygons.gpc.poly(int.man2[[i]], 
                paste(city$place[1], i, sep = ""))
        }
        ply.city.blk <- SpatialPolygons(city.blk, proj4string = CRS("+proj=longlat +datum=NAD83"))
        m2 <- m[intersect.index, ]
        area1 <- sapply(slot(ply.city.blk, "polygons"), slot, 
            "area")
        area2 <- sapply(slot(m2, "polygons"), slot, "area")
        names.of.cols <- names(temp.level)[which(names(temp.level) == 
            "pop2000"):length(names(temp.level))]
        tempvalue <- as.numeric(m2@data[, names.of.cols[1]])
        newvalue <- ceiling((area1/area2) * tempvalue)
        for (i in 2:length(names.of.cols)) {
            tempvalue <- as.numeric(m2@data[, names.of.cols[i]])
            newvalue <- cbind(newvalue, ceiling((area1/area2) * 
                tempvalue))
        }
        if (level == "tract") {
            temp <- data.frame(cbind(m2$state, m2$county, m2$tract, 
                newvalue), stringsAsFactors = FALSE)
        }else if (level == "blkgrp") {
            temp <- data.frame(cbind(m2$state, m2$county, m2$tract, 
                m2$blkgrp, newvalue), stringsAsFactors = FALSE)
        }else {
            temp <- data.frame(cbind(m2$fips, newvalue), stringsAsFactors = FALSE)
        }
        colnames(temp) <- names(m2)
        rownames(temp) <- sapply(slot(ply.city.blk, "polygons"), 
            slot, "ID")
        out <- SpatialPolygonsDataFrame(ply.city.blk, data = temp)
        if (is.null(proj) == FALSE) {
            require(rgdal)
            out <- spTransform(out, proj)
        }
        out
    }
poly.clipper.aux(name, state, statefips, level, bb.epsilon, sp.object, proj)
}
