`plotRules` <-
function(rules, zcolor = "bw", ellipse = FALSE, transparency = 0.3, ...) {
    
    # s - sets; v - version; b - borders; x,y - coordinates
    borders <- read.csv(file.path(system.file("data", package="venn"), "borders.csv.gz"))
    
    # s - sets; v - version; n - set number; x,y - coordinates
    sets <- read.csv(file.path(system.file("data", package="venn"), "sets.csv.gz"))
    
    zeroset <- matrix(c(0, 1000, 1000, 0, 0, 0, 0, 1000, 1000, 0), ncol = 2)
    colnames(zeroset) <- c("x", "y")
    
    default <- identical(zcolor, "style")
    
    # assume the zones cover all sets (if rules is a number that is TRUE by default, anyways)
    allsets <- TRUE
    
    if (is.character(rules)) {
        
        if (identical(zcolor, "bw")) {
            zcolor <- rep("#96bc72", length.out = length(rules))
        }
        else if (identical(zcolor, "style")) {
            zcolor <- colorRampPalette(c("red", "blue", "green", "yellow"))(length(rules))
        }
        else {
            zcolor <- rep(zcolor, length.out = length(rules))
        }
        
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
        
        
        # check if any of the remaining rowns define a whole set
            
        # wholesets will be a numeric vector:
        # -1 if it's not a whole set
        # the number of the set (if it is whole), from the order in the truth table
        
        wholesets <- unlist(lapply(rowns, function(x) {
            x <- which(apply(tt, 2, function(y) {
                identical(x, as.numeric(rownames(tt)[y == 1]))
            }))
            
            return(ifelse(length(x) > 0, x, -1))
        }))
        
        allwhole <- all(wholesets > 0)
        
        # verify if the rules cover all sets
        allsets <- length(rules) == nofsets & allwhole
        
        
        if (nofsets < 4 | nofsets > 5) {
            ellipse <- FALSE
        }
        
        zones <- vector("list", length(wholesets))
        
        regular <- !unlist(lapply(rowns, identical, 0))
        
        if (any(!regular)) { # inverse, the area outside all sets
            zones[[which(!regular)]] <- getZones(0, nofsets, ellipse)
            polygons <- rbind(zeroset, rep(NA, 2), zones[[which(!regular)]][[1]])
            polygons <- polygons[-nrow(polygons), ] # needed...?
            polypath(polygons, rule = "evenodd", col = adjustcolor(zcolor[!regular], alpha.f = transparency), border = NA)
        }
        
        
        if (any(regular)) {
            
            if (any(wholesets > 0)) {
                
                for (i in which(wholesets > 0)) {
                    # [[1]] simulez getZones() pentru ca uneori pot fi mai multe zone
                    zones[[i]][[1]] <- sets[sets$s == nofsets & sets$v == as.numeric(ellipse) & sets$n == wholesets[i], c("x", "y")]
                }
            }
            
            if (any(wholesets < 0)) {
                for (i in which(wholesets < 0)) {
                    zones[[i]] <- getZones(rowns[[i]], nofsets, ellipse)
                }
            }
            
            
            for (i in seq(length(zones))) {
                if (regular[i]) {
                    for (j in seq(length(zones[[i]]))) {
                        polygon(zones[[i]][[j]], col = adjustcolor(zcolor[i], alpha.f = transparency), border = NA)
                    }
                }
            }
        }
    }
    else { # is numeric
        nofsets <- rules
        allsets <- TRUE
        allwhole <- TRUE
        
        if (identical(zcolor, "style")) {
            zcolor <- colorRampPalette(c("red", "yellow", "green", "blue"))(nofsets)
        }
        else if (!identical(zcolor, "bw")) {
            zcolor <- rep(zcolor, length.out = nofsets)
        }
    }
    
    
    
    if (nofsets < 4 | nofsets > 5) {
        ellipse <- FALSE
    }
    
    
    
    other.args <- list(...)
    
    lines(zeroset)
    
    if (!identical(zcolor, "bw")) {
        # border colors, a bit darker
        bcolor <- rgb(t(col2rgb(zcolor)/1.4), maxColorValue=255)
    }
    else {
        bcolor <- "#000000"
    }
    
    if (allsets & allwhole) {
        
        if (is.numeric(rules) & !identical(zcolor, "bw")) {
            # the zones have not been plotted yet
            
            polygon(sets[sets$s == nofsets & sets$v == as.numeric(ellipse), c("x", "y")],
                    col = adjustcolor(zcolor, alpha.f = transparency), border = NA)
            
        }
        
        # now the borders
        
        
        if (default) {
            
            # the default set of colors ignores all other additional parameters for the borders
            
            for (i in seq(nofsets)) {
                suppressWarnings(
                    lines(sets[sets$s == nofsets & sets$v == as.numeric(ellipse) & sets$n == i, c("x", "y")], col = bcolor[i])
                )
            }
            
        }
        else {
            
            if (length(other.args) > 0) {
                
                if (any(sapply(other.args, length) > 1)) {
                    
                    # there are different border colors for each zone (set in this case) 
                    
                    # arguments are recycled to the length of the zones
                    other.args <- lapply(other.args, function(x) {
                        if (length(x) != nofsets) {
                            return(rep(x, length.out = nofsets))
                        }
                        else {
                            return(x)
                        }
                    })
                    
                    
                    for (i in seq(nofsets)) {
                        
                        seplines <- list(as.name("lines"), x = sets[sets$s == nofsets & sets$v == as.numeric(ellipse) & sets$n == i, c("x", "y")])
                        suppress <- list(as.name("suppressWarnings"))
                        
                        for (j in names(other.args)) {
                            seplines[[j]] <- other.args[[j]][i]
                        }
                        
                        suppress[[2]] <- as.call(seplines)
                        
                        eval(as.call(suppress))
                    
                    }
                    
                }
                else {
                    
                    suppressWarnings(lines(sets[sets$s == nofsets & sets$v == as.numeric(ellipse), c("x", "y")], ... = ...))
                    
                }
            }
            else {
                # print borders in black
                suppressWarnings(lines(sets[sets$s == nofsets & sets$v == as.numeric(ellipse), c("x", "y")]))
            }
        }
    }
    else {
        
        # first print all borders in black
        # (important to begin with this, the zones might not cover all intersections)
        suppressWarnings(lines(sets[sets$s == nofsets & sets$v == as.numeric(ellipse), c("x", "y")]))
        
        # surely this is not numeric, there are zones already calculated
        
        
        if (default) {
            
            for (i in seq(length(zones))) {
                for (j in seq(length(zones[[i]]))) {
                    suppressWarnings(lines(zones[[i]][[j]], col = bcolor[i]))
                }
            }
            
        }
        else {
            
            if (length(other.args) > 0) {
                
                # arguments are recycled to the length of the zones
                other.args <- lapply(other.args, function(x) {
                    if (length(x) != length(rules)) {
                        return(rep(x, length.out = length(rules)))
                    }
                    else {
                        return(x)
                    }
                })
                
                
                for (i in seq(length(zones))) {
                    
                    for (j in seq(length(zones[[i]]))) {
                
                        seplines <- list(as.name("lines"), x = zones[[i]][[j]])
                        suppress <- list(as.name("suppressWarnings"))
                        
                        for (j in names(other.args)) {
                            seplines[[j]] <- other.args[[j]][i]
                        }
                        
                        suppress[[2]] <- as.call(seplines)
                        
                        eval(as.call(suppress))
                        
                    }
                
                }
                
            }
            
        }
        
    }
    
}

    
