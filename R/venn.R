`venn` <-
function(x = "", size = 15, ilabels = FALSE, counts = FALSE, snames = c(""),
         zcolor = c(""), transparency = 1, ...) {
    
    if (missing(x)) {
        cat("\n")
        stop("Argument \"x\" is missing.\n\n", call. = FALSE)
    }
    
    if (is.numeric(x) & length(x) > 1) {
        cat("\n")
        stop("Argument \"x\" can be a single digit, for the number of sets.\n\n", call. = FALSE)
    }
    
    
    if (inherits(x, "qca") | inherits(x, "tt")) {
        if (inherits(x, "tt")) {
            tt <- x$tt
            snames <- unlist(strsplit(gsub("[[:space:]]", "", x$options$conditions), split = ","))
        }
        else {
            tt <- x$tt$tt
            snames <- unlist(strsplit(gsub("[[:space:]]", "", x$tt$options$conditions), split = ","))
        }
        
        nofsets <- length(snames)
        
        ttcolors <- c(
            "0" = "#ffd885",
            "1" = "#96bc72",
            "C" = "#1c8ac9",
            "?" = "#ffffff" # white
        )
        
        openPlot(size)
        
        ints <- read.csv(file.path(system.file("data", package="venn"), "ints.csv.gz"))

        for (i in names(ttcolors)[1:3]) {
            
            zones <- as.numeric(rownames(tt[tt$OUT == i, ])) - 1
            
            if (any(zones == 0)) {
                
                zeroset <- matrix(c(0, 1000, 1000, 0, 0, 0, 0, 1000, 1000, 0), ncol = 2)
                colnames(zeroset) <- c("x", "y")
                
                polygons <- rbind(zeroset, rep(NA, 2), getZones(0, nofsets)[[1]])
                
                polygons <- polygons[-nrow(polygons), ]
                
                polypath(polygons, rule = "evenodd", col = ttcolors[i], border = NA)
                
                zones <- zones[-1]
            }
            
            polygon(ints[ints$s == nofsets & ints$i %in% zones, 3:4], col = ttcolors[i])
            
        }
        
        for (i in 0:3) {
            polygon(110*i + c(0, 19, 19, 0), c(0, 0, 19, 19) - 35, col = ttcolors[i + 1])
            text(110*i + 40, 9 - 35, names(ttcolors)[i + 1], cex = 0.85)
        }
        
        cts <- tt$n
        x <- nofsets
        
    }
    else if (is.numeric(x)) {
        nofsets <- x
        openPlot(size)
    }
    else if (is.character(x)) {
        x <- gsub("[[:space:]]", "", x)
        
        if (!all(gsub("0|1|-|\\+", "", x) == "")) {
            cat("\n")
            stop("Invalid values in the rule(s).\n\n", call. = FALSE)
        }
        
        x <- unlist(strsplit(x, split="\\+"))
        nofsets <- unique(nchar(x))
        openPlot(size)
    }
    else if (is.data.frame(x)) {
        
        if (!is.null(names(x))) {
            if (all(names(x) != "")) {
                snames <- names(x)
            }
        }
        
        if (!all(apply(x, 1, function(l) all(l %in% 0:1)))) {
            cat("\n")
            stop("As a dataframe, \"x\" can only contain values 0 and 1.\n\n", call. = FALSE)
        }
        
        nofsets <- length(x)
        
        tt <- sapply(rev(seq(nofsets)), function(x) {
            rep.int(c(sapply(0:1, function(y) rep.int(y, 2^(x - 1)))),
                    2^nofsets/2^x)
        })
        
        cts <- apply(tt, 1, function(l1) {
            sum(apply(x, 1, function(l2) {
                all(l1 == l2)
            }))
        })
        
        counts <- TRUE
        x <- nofsets
        openPlot(size)
    }
    else if (is.list(x)) {
        
        if (length(x) > 7) {
            x <- x[seq(7)]
        }
        
        if (!is.null(names(x))) {
            if (all(names(x) != "")) {
                snames <- names(x)
            }
        }
        
        nofsets <- length(x)
        
        tt <- sapply(rev(seq(nofsets)), function(x) {
            rep.int(c(sapply(0:1, function(y) rep.int(y, 2^(x - 1)))),
                    2^nofsets/2^x)
        })
        
        cts <- c(0, unlist(lapply(seq(2, nrow(tt)), function(l) {
            l <- which(tt[l, ] == 1)
            if (length(l) == 1) {
                return(length(setdiff(x[[l]], unique(unlist(x[-l])))))
            }
            else {
                int <- intersect(x[[l[1]]], x[[l[2]]])
                if (length(l) > 2) {
                    for (i in seq(3, length(l))) {
                        int <- intersect(int, x[[l[i]]])
                    }
                }
                return(length(int))
            }
        })))
        
        counts <- TRUE
        x <- nofsets
        openPlot(size)
    }
    else {
        cat("\n")
        stop("Unrecognised argument \"x\".\n\n", call. = FALSE)
    }
    
    if (nofsets > 7) {
        cat("\n")
        stop("Venn diagrams can be drawn up to 7 sets only.\n\n", call. = FALSE)
    }
    
    if (identical(snames, "")) {
        snames <- LETTERS[seq(nofsets)]
    }
    else {
        if (length(snames) != x) {
            cat("\n")
            stop("Length of snames does not match the number of sets.\n\n", call. = FALSE)
        }
    }
    
    
    if (!identical(zcolor, "")) {
        zcolor <- gsub("\\n", "", unlist(strsplit(gsub("[[:space:]]", "", zcolor), split = ",")))
        zcolor <- adjustcolor(zcolor, alpha.f = transparency)
    }
    
    
    plotRules(x, zcolor, ...=...)
    
    scoords <- data.frame(
        s = c(1, rep(2, 2), rep(3, 3), rep(4, 4), rep(5, 5), rep(6, 6), rep(7, 7)),
        x = c(500, 250, 750, 100, 500, 900,  88, 263, 713, 888,  88, 533, 850, 750, 163, 100, 500, 910, 925, 550, 100, 220, 685, 935, 935, 600, 155,  50),
        y = c(780, 780, 780, 560, 910, 560, 663, 850, 850, 663, 750, 963, 688,  40,  88, 860, 975, 775, 165,  30, 140, 955, 980, 780, 200,  15, 120, 690)
    )
    
    if (ilabels | counts) {
        icoords <- read.csv(file.path(system.file("data", package="venn"), "icoords.csv.gz"))
        ilabels <- icoords$l[icoords$s == nofsets]
        if (counts) {
            cts[cts == 0] <- ""
            ilabels <- cts
        }
        text(icoords[icoords$s == nofsets, 2:3], labels = ilabels, cex = 0.45)
    }
        
    text(scoords[scoords$s == nofsets, 2:3], labels = snames, cex = 0.85)
    
}


