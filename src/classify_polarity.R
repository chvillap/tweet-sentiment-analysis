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

classify_polarity <- function(docs, algorithm = "voter") {
    # Voter algorithm.
    # Simply counts the number of positive and negative words, compute the
    # positive/negative ratio and use the result to classify each document.
    if (algorithm == "voter") {
        df_lexicon <- read.csv("data/SentiLex-flex-PT02-clean-voter.csv")

        predicts <- lapply(docs, function (doc) {
            words <- unlist(str_split(doc, "\\s+"))
            pos_score <- 0
            neg_score <- 0
            for (w in words) {
                polarity <- df_lexicon[df_lexicon$LEM == w, "POL"]
                if (length(polarity) > 0) {
                    if (polarity > 0)
                        pos_score <- pos_score + polarity
                    else
                        neg_score <- neg_score + polarity
                }
            }
            list(POS = pos_score, NEG = neg_score,
                 RATIO = (pos_score + 1e-10) / (neg_score + 1e-10))
        })
    
        df_results <- data.frame(POS = numeric(n), NEG = numeric(n),
                                 RATIO = numeric(n), BEST_FIT = character(n),
                                 stringsAsFactors = FALSE)
        for (j in 1:n) {
            pred <- predicts[[j]]
            best_fit <- ifelse(pred$RATIO > 1, "positive",
                        ifelse(pred$RATIO < 1, "negative", "neutral"))
            df_results[j,] <- list(pred$POS, pred$NEG, pred$RATIO, best_fit)
        }
        df_results$BEST_FIT <- factor(df_results$BEST_FIT)
        df_results
    }
    # Naive Bayes algorithm.
    # Uses a pre-trained probabilistic model to define the most suited class
    # for each document.
    else if (algorithm == "bayes") {
        source("src/nbayes.R")

        model <- readRDS("model/nbayes.rds")
        predicts <- nbayes.predict(docs, model$PRIORI, model$POSTERIORI)

        df_results <- data.frame(POSITIVE = numeric(n), NEGATIVE = numeric(n),
                                 NEUTRAL = numeric(n), BEST_FIT = character(n),
                                 stringsAsFactors = FALSE)
        for (j in 1:n) {
            pred <- predicts[[j]]
            df_results[j,] <- list(pred$PROBABILITY["positive"],
                                   pred$PROBABILITY["negative"],
                                   pred$PROBABILITY["neutral"],
                                   pred$BEST_FIT)
        }
        df_results$BEST_FIT <- factor(df_results$BEST_FIT)
        df_results        
    }
}
