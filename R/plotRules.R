`plotRules` <-
function(rules, zcolor = c(""), ...) {
    
    # s - sets; b - borders; x,y - coordinates
    borders <- read.csv(file.path(system.file("data", package="venn"), "borders.csv.gz"))
    
    zeroset <- matrix(c(0, 1000, 1000, 0, 0, 0, 0, 1000, 1000, 0), ncol = 2)
    
    if (identical(zcolor, "")) {
        zcolor <- "#96bc72"
    }
    
    
    if (is.character(rules)) {
        
        zcolor <- rep(zcolor, length.out = length(rules))
        
        nofsets <- unique(nchar(rules))
        
        tt <- sapply(rev(seq(nofsets)), function(x) {
            rep(c(sapply(0:1, function(y) rep(y, 2^(x - 1)))), 2^nofsets/2^x)
        })
        
        rownames(tt) <- seq(nrow(tt)) - 1
        
        rowns <- lapply(rules, function(x) {
            x <- unlist(strsplit(x, split=""))
            ttc <- tt
            for (j in seq(length(x))) {
                if (x[j] != "-") {
                    ttc <- subset(ttc, ttc[, j] == x[j])
                }
            }
            return(as.numeric(rownames(ttc)))
        })
        
        
        if (any(inverse <- unlist(lapply(rowns, function(x) any(x == 0))))) {
            zonesinv <- lapply(rowns[inverse], function(x) getZones(x, nofsets))
            rowns <- rowns[!inverse]
            
            for (i in seq(length(zonesinv))) {
                polygons <- rbind(zeroset, rep(NA, 2))
                
                for (j in seq(length(zonesinv[[i]]))) {
                    polygons <- rbind(polygons, zonesinv[[i]][[j]])
                }
                
                polygons <- polygons[-nrow(polygons), ]
                
                polypath(polygons, rule = "evenodd", col = zcolor[which(inverse)[i]], border = NA)
            }
        }
        
        
        if (length(rowns) > 0) {
            zones <- lapply(rowns, function(x) getZones(x, nofsets))
            polygons <- matrix(ncol = 2, nrow = 0)
            
            for (i in seq(length(zones))) {
                for (j in seq(length(zones[[i]]))) {
                    polygons <- rbind(polygons, zones[[i]][[j]])
                }
            }
            
            polygon(polygons, col = rep(zcolor[!inverse], unlist(lapply(zones, length))), border = NA)
        }
    }
    else {
        nofsets <- rules
    }
    
    lines(zeroset)
    lines(borders[borders$s == nofsets, 3:4], ...=...)
    
}

    
