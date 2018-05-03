# Copyright (c) 2018, Adrian Dusa
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, in whole or in part, are permitted provided that the
# following conditions are met:
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#     * The names of its contributors may NOT be used to endorse or promote products
#       derived from this software without specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL ADRIAN DUSA BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

`plotRules` <-
function(rules, zcolor = "bw", ellipse = FALSE, opacity = 0.3, allborders = TRUE, ...) {
    borders <- read.csv(system.file("data", "borders.csv.gz", package = "venn"))
    sets <- read.csv(system.file("data", "sets.csv.gz", package = "venn"))
    zeroset <- matrix(c(0, 1000, 1000, 0, 0, 0, 0, 1000, 1000, 0), ncol = 2)
    colnames(zeroset) <- c("x", "y")
    default <- identical(zcolor, "style")
    allsets <- TRUE
    if (is.list(rules)) {
        if (identical(zcolor, "bw")) {
            zcolor <- rep("#96bc72", length.out = length(rules))
        }
        else if (identical(zcolor, "style")) {
            zcolor <- colorRampPalette(c("red", "blue", "green", "yellow"))(length(rules))
        }
        else {
            zcolor <- rep(zcolor, length.out = length(rules))
        }
        nofsets <- unique(unlist(lapply(rules, function(x) {
            nchar(unlist(strsplit(x, split = "\\+")))
        })))    
        tt <- sapply(rev(seq(nofsets)), function(x) {
            rep(c(sapply(0:1, function(y) rep(y, 2^(x - 1)))), 2^nofsets/2^x)
        })
        rownames(tt) <- seq(nrow(tt)) - 1
        rowns <- lapply(rules, function(x) {
            sort(unique(unlist(lapply(strsplit(x, split = "\\+"), function(x) {
                unlist(lapply(strsplit(x, split = ""), function(x) {
                    ttc <- tt
                    for (j in seq(length(x))) {
                        if (x[j] != "-") {
                            ttc <- subset(ttc, ttc[, j] == x[j])
                        }
                    }
                    return(as.numeric(rownames(ttc)))
                }))
            }))))
        })
        wholesets <- unlist(lapply(rules, function(x) {
            ifelse(nchar(gsub("-", "", x)) == 1, as.vector(regexpr("[0-9]", x)), 0)
        }))
        allwhole <- all(wholesets > 0)
        allsets <- length(rules) == nofsets & allwhole
        if (nofsets < 4 | nofsets > 5) {
            ellipse <- FALSE
        }
        zones <- vector("list", length(wholesets))
        iregular <- unlist(lapply(rowns, function(x) any(x == 0)))
        if (any(iregular)) { 
            for (i in which(iregular)) {
                zones[[i]] <- getZones(rowns[[i]], nofsets, ellipse)
                polygons <- rbind(zeroset, rep(NA, 2), zones[[i]][[1]])
                polygons <- polygons[-nrow(polygons), ] 
                polypath(polygons, rule = "evenodd", col = adjustcolor(zcolor[i], alpha.f = opacity), border = NA)
            }
        }
        if (any(!iregular)) { 
            if (any(wholesets > 0)) {
                for (i in which(wholesets > 0)) {
                    zones[[i]][[1]] <- sets[sets$s == nofsets & sets$v == as.numeric(ellipse) & sets$n == wholesets[i], c("x", "y")]
                }
            }
            if (any(wholesets == 0)) {
                for (i in which(wholesets == 0 & !iregular)) {
                    zones[[i]] <- getZones(rowns[[i]], nofsets, ellipse)
                }
            }
            for (i in seq(length(zones))) {
                if (!iregular[i]) {
                    for (j in seq(length(zones[[i]]))) {
                        polygon(zones[[i]][[j]], col = adjustcolor(zcolor[i], alpha.f = opacity), border = NA)
                    }
                }
            }
        }
    }
    else if (is.numeric(rules)) {
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
    else {
        cat("\n")
        stop(simpleError("Something went wrong\n\n"))
    }
    if (nofsets < 4 | nofsets > 5) {
        ellipse <- FALSE
    }
    other.args <- list(...)
    lines(zeroset)
    if (!identical(zcolor, "bw")) {
        bcolor <- rgb(t(col2rgb(zcolor)/1.4), maxColorValue = 255)
    }
    else {
        bcolor <- "#000000"
    }
    if (allsets & allwhole) {
        if (is.numeric(rules) & !identical(zcolor, "bw")) {
            polygon(sets[sets$s == nofsets & sets$v == as.numeric(ellipse), c("x", "y")],
                    col = adjustcolor(zcolor, alpha.f = opacity), border = NA)
        }
        if (default) {
            for (i in seq(nofsets)) {
                suppressWarnings(
                    lines(sets[sets$s == nofsets & sets$v == as.numeric(ellipse) & sets$n == i, c("x", "y")], col = bcolor[i])
                )
            }
        }
        else {
            if (length(other.args) > 0) {
                if (any(sapply(other.args, length) > 1)) {
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
                suppressWarnings(lines(sets[sets$s == nofsets & sets$v == as.numeric(ellipse), c("x", "y")]))
            }
        }
    }
    else {
        if (allborders) {
            suppressWarnings(lines(sets[sets$s == nofsets & sets$v == as.numeric(ellipse), c("x", "y")]))
        }
        else {
            if (!is.element("col", names(other.args))) {
                other.args$col <- "black"
            }
        }
        if (default) {
            for (i in seq(length(zones))) {
                for (j in seq(length(zones[[i]]))) {
                    suppressWarnings(lines(zones[[i]][[j]], col = bcolor[i]))
                }
            }
        }
        else {
            if (length(other.args) > 0) {
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
                        if (any(names(other.args) == "col")) {
                            other.args$col <- unlist(strsplit(gsub("[[:space:]]", "", other.args$col), split = ","))
                        }
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
