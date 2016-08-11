# MIT License
#
# Copyright (c) 2016 Carlos Henrique Villa Pinto
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

library(stringr)

# -----------------------------------------------------------------------------
# Naive Bayes training

nbayes.train <- function(docs, targets, priori = NULL) {
    target_levels <- levels(targets)
    vocabulary <- unique(unlist(str_split(docs, "\\s+")))

    texts <- sapply(target_levels, function(l) {
        paste(docs[targets == l], collapse = " ")
    })
    counts <- str_count(texts, "\\w+")
    names(counts) <- target_levels

    if (is.null(priori)) {
        priori <- sapply(target_levels, function(l) {
            length(docs[targets == l]) / length(docs)
        })
        names(priori) <- target_levels
    }
    posteriori <- matrix(0, nrow = length(vocabulary),
                            ncol = length(target_levels),
                            dimnames = list(vocabulary, target_levels))
    for (l in target_levels) {
        posteriori[, l] <- sapply(vocabulary, function (w) {
            count_w <- str_count(texts[l], w)
            (count_w + 1) / (counts[l] + length(vocabulary))
        })
    }
    # posteriori <- outer(vocabulary, target_levels, function (w, l) {
    #     count_w <- str_count(texts[l], w)
    #     (count_w + 1) / (counts[l] + length(vocabulary))
    # })
    # rownames(posteriori) <- vocabulary
    # colnames(posteriori) <- target_levels
    
    return(list(PRIORI = priori, POSTERIORI = posteriori))
}

# -----------------------------------------------------------------------------
# Naive Bayes prediction

nbayes.predict <- function(docs, priori, posteriori) {
    vocabulary <- rownames(posteriori)
    target_levels <- colnames(posteriori)

    results <- lapply(docs, function(doc) {
        words <- unlist(str_split(doc, "\\s+"))
        probability <- sapply(target_levels, function(l) {
            priori[l] * prod(sapply(words, function(w) {
                ifelse(w %in% vocabulary, posteriori[w, l], 1)
            }))
        })
        names(probability) <- target_levels
        predict <- target_levels[which.max(probability)]
        
        list(PROBABILITY = probability, BEST_FIT = predict)
    })
    names(results) <- NULL
    return(results)
}
