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

`translate2` <-
function(expression = "", snames = "") {
    if (identical(expression, "")) {
        cat("\n")
        stop(simpleError("Empty expression.\n\n"))
    }
    if (grepl("<=>", expression) | grepl("=>", expression) | grepl("<=", expression)) {
        cat("\n")
        stop(simpleError("Incorrect expression.\n\n"))
    }
    if (!is.vector(snames)) {
        cat("\n")
        stop(simpleError("Set names should be a single string or a vector of names.\n\n"))
    }
    getRow <- function(noflevels, row.no, zerobased=FALSE) {
        max.combs <- prod(noflevels)
        if (any(row.no > (max.combs - zerobased))) {
            cat("\n")
            stop(simpleError(sprintf("There cannot be more than %s rows.\n\n", max.combs)))
        }
        if (!zerobased) {row.no <- row.no - 1}
        mbase <- c(rev(cumprod(rev(noflevels))), 1)[-1]
        t(sapply(row.no, function(x) x %/% mbase) %% noflevels)
    }
    trimst <- function(x) {
        gsub("^[[:space:]]+|[[:space:]]+$", "", x)
    }
    splitstr <- function(x) {
        gsub("\\n", "", unlist(strsplit(gsub("[[:space:]]", "", x), split = ",")))
    }
    createMatrix <- function(noflevels, logical = FALSE) {
        conds <- length(noflevels)
        pwr <- unique(noflevels)
        if (any(pwr > 2)) {
            logical <- FALSE
        }
        if (length(pwr) == 1) {
            create <- function(idx) {
                rep.int(c(sapply(seq_len(pwr) - 1, function(x) rep.int(x, pwr^(idx - 1)))),
                        pwr^conds/pwr^idx)
            }
            retmat <- sapply(rev(seq_len(conds)), create)
        }
        else {
            mbase <- c(rev(cumprod(rev(noflevels))), 1)[-1]
            orep  <- cumprod(rev(c(rev(noflevels)[-1], 1)))
            retmat <- sapply(seq_len(conds), function(x) {
               rep.int(rep.int(seq_len(noflevels[x]) - 1, rep.int(mbase[x], noflevels[x])), orep[x])
            })
        }
        if (logical) {
            retmat <- matrix(as.logical(retmat), nrow=nrow(retmat), ncol=ncol(retmat))
        }
        if (is.vector(retmat)) {
            retmat <- matrix(retmat, nrow=1)
        }
        return(retmat)
    }
    if (grepl("\\{|\\}", expression)) {
        cat("\n")
        stop(simpleError("Incorrect expression.\n\n"))
    }
    if (!identical(snames, "")) {
        snames <- toupper(splitstr(snames))
    }
    pporig <- trimst(unlist(strsplit(gsub("\\(|\\)", "", expression), split="\\+")))
    expression <- gsub("[[:space:] \\( \\)]", "", expression)
    tempexpr <- gsub("\\*|,|;", "", expression)
    pp <- unlist(strsplit(expression, split="\\+"))
    if (grepl("\\*", expression)) {
        conds <- sort(unique(toupper(gsub("~", "", unlist(strsplit(pp, split="\\*"))))))
        if (!identical(snames, "")) {
            if (all(gsub("~", "", conds) %in% snames)) {
                conds <- snames
            }
            else {
                cat("\n")
                stop(simpleError("Parts of the expression don't match the set names from \"snames\" argument.\n\n"))
            }
        }
        retmat <- matrix(0, nrow=length(pp), ncol=length(conds))
        rownames(retmat) <- pporig
        colnames(retmat) <- conds
        negation <- retmat
        for (i in seq_along(pp)) {
            x <- unlist(strsplit(pp[i], split="\\*"))
            cols <- toupper(gsub("~", "", x))
            if (any(duplicated(cols))) {
                cat("\n")
                stop(simpleError("Duplicated names in the same product.\n\n"))
            }
            if (!all(cols %in% conds)) {
                cat("\n")
                stop(simpleError("Parts of the expression don't match the set names from \"snames\" argument.\n\n"))
            }
            cols <- match(cols, conds)
            retmat[i, cols] <- as.numeric(gsub("~", "", x) %in% conds) + 1
            negation[i, cols] <- as.numeric(grepl("~", x))
        }
    }
    else {
        conds <- unique(toupper(gsub("~", "", pp)))
        if (all(nchar(conds) == 1)) {
            if (!identical(snames, "")) {
                if (all(conds %in% snames)) {
                    conds <- snames
                }
                else {
                    cat("\n")
                    stop(simpleError("Parts of the expression don't match the set names from \"snames\" argument.\n\n"))
                }
            }
            retmat <- matrix(0, nrow=length(pp), ncol=length(conds))
            rownames(retmat) <- pporig
            colnames(retmat) <- conds
            negation <- retmat
            for (i in seq_along(pp)) {
                retmat[i, match(toupper(gsub("~", "", pp[i])), conds)] <- as.numeric(gsub("~", "", pp[i]) %in% conds) + 1
                negation[i, match(toupper(gsub("~", "", pp[i])), conds)] <- as.numeric(grepl("~", pp[i]))
            }
        }
        else {
            if (identical(snames, "")) {
                cat("\n")
                stop(simpleError("Unable to translate without the set name(s).\n\n"))
            }
            retmat <- matrix(0, nrow=length(pp), ncol=length(snames))
            rownames(retmat) <- pporig
            colnames(retmat) <- snames
            negation <- retmat
            if (all(toupper(gsub("~", "", pp)) %in% snames)) {
                for (i in seq_along(pp)) {
                    conds <- match(toupper(gsub("~", "", pp[i])), snames)
                    retmat[i, conds] <- as.numeric(gsub("~", "", pp[i]) %in% snames) + 1
                    negation[i, conds] <- as.numeric(grepl("~", pp[i]))
                }
            }
            else {
                if (all(nchar(snames) == 1)) {
                    for (i in seq_along(pp)) {
                        x <- unlist(strsplit(pp[i], split=""))
                        if (any(x == "~")) {
                            y <- which(x == "~")
                            if (max(y) == length(x)) {
                                cat("\n")
                                stop(simpleError("Incorrect expression, tilde not in place.\n\n"))
                            }
                            x[y + 1] <- paste("~", x[y + 1], sep="")
                            x <- x[-y]
                        }
                        cols <- toupper(gsub("~", "", x))
                        if (any(duplicated(cols))) {
                            cat("\n")
                            stop(simpleError("Duplicated names in the same product.\n\n"))
                        }
                        if (!all(cols %in% snames)) {
                            cat("\n")
                            stop(simpleError("Parts of the expression don't match the set names from \"snames\" argument.\n\n"))
                        }
                        for (j in seq_along(x)) {
                            retmat[i, match(cols[j], snames)] <- as.numeric(gsub("~", "", x[j]) %in% snames) + 1
                            negation[i, match(cols[j], snames)] <- as.numeric(grepl("~", x[j]))
                        }
                    }
                }
                else {
                    maybe <- logical(length(snames))
                    for (i in seq_along(snames)) {
                        maybe[i] <- grepl(snames[i], toupper(expression))
                    }
                    snames <- snames[maybe]
                    if (length(snames) > 7) {
                        cat("\n")
                        stop(simpleError("Too many causal snames' to search.\n\n"))
                    }
                    im <- createMatrix(rep(3, length(snames)))[-1, , drop = FALSE]
                    mns <- matrix(nrow = 0, ncol = ncol(im))
                    perms <- function(x) {
                        if (length(x) == 1) {
                            return(x)
                        }
                        else {
                            res <- matrix(nrow = 0, ncol = length(x))
                            for(i in seq_along(x)) {
                                res <- rbind(res, cbind(x[i], Recall(x[-i])))
                            }
                            return(res)
                        }
                    }
                    noflevels <- rep(3, length(snames))
                    mns <- lapply(seq(2, 3^length(snames)), function(x) {
                        x <- getRow(noflevels, x)
                        snames[x == 1] <- tolower(snames[x == 1])
                        snames <- snames[x > 0]
                        if (length(snames) > 1) {
                            return(perms(snames))
                        }
                        else {
                            return(matrix(snames, 1, 1))
                        }
                    })
                    namespace <- matrix(nrow=0, ncol=1)
                    for (i in seq(length(mns))) {
                        temp <- matrix(apply(mns[[i]], 1, paste, collapse=""), ncol=1)
                        rownames(temp) <- paste(i, seq(nrow(temp)), sep="_")
                        namespace <- rbind(namespace, temp)
                    }
                    if (any(duplicated(namespace))) {
                        cat("\n")
                        stop(simpleError("Impossible to translate: set names clash.\n\n"))
                    }
                    matched <- match(gsub("~", "", pp), namespace)
                    if (any(is.na(matched))) {
                        cat("\n")
                        stop(simpleError("Incorrect expression, unknown set names.\n\n"))
                    }
                    matched <- rownames(namespace)[matched]
                    retmat <- matrix(0, length(pp), length(snames))
                    rownames(retmat) <- pporig
                    colnames(retmat) <- snames
                    negation <- retmat
                    for (i in seq(length(matched))) {
                        id <- as.numeric(unlist(strsplit(matched[i], split="_")))
                        x <- mns[[id[1]]][id[2], ]
                        y <- rep(-1, length(snames))
                        y[match(toupper(x), snames)] <- as.numeric(x %in% snames)
                        retmat[i, ] <- y + 1
                    }
                    for (i in seq_along(pp)) {
                        cplus <- which(retmat[i, ] != "")
                        for (j in cplus) {
                            tildefirst <- substring(pp[i], 1, 1) == "~"
                            negation[i, j] <- as.numeric(tildefirst)
                            pp[i] <- substring(pp[i], nchar(snames[j]) + 1 + 1*tildefirst)
                        }
                    }
                }
            }
        }
    }
    for (i in seq(length(retmat))) {
        if (negation[i] == 1) {
            retmat[i] <- 3 - as.numeric(retmat[i])
        }
    }
    return(structure(retmat - 1, class = "translate"))
}
