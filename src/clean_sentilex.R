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

library(readr)
library(stringr)
library(dplyr)
library(tidyr)

# Read the SentiLex-flex-PT02 dataset.
df <- as.data.frame(read_lines("data/SentiLex-PT02/SentiLex-flex-PT02.txt"))
names(df) <- c("DATA")

# Separate the lemmas (flexed, unflexed forms) from the rest.
df <- separate(df, "DATA", c("LEM", "DATA"), sep = "\\.")

# Remove the unflexed forms of the lemmas.
df$LEM <- gsub("(.+),.+", "\\1", df$LEM)

# Remove lemmas composed of two or more words (idiomatic expressions).
idioms_ind <- grepl("\\s+", df$LEM)
df <- subset(df, !idioms_ind)

# Find lemmas that can have different meanings according to the context.
mults_ind <- grepl("TG=HUM:N\\d:N\\d", df$DATA)
mults <- subset(df, mults_ind)

# For now, replace their polarities by their sum.
# More will be done later.
as.numeric.factor <- function(x) {
    as.numeric(levels(x))[x]
}
mults$DATA <- sapply(mults$DATA, function(x) {    
    pattern <- "POL:N0=(-?\\d);POL:N1=(-?\\d);"
    matches <- data.frame(str_match(x, pattern))
    n <- as.numeric.factor(matches[, 2]) + as.numeric.factor(matches[, 3])
    gsub(pattern, paste("POL:N0:N1=", n, ";", sep = ""), x)
})
df[mults_ind, ] <- mults

# Split the DATA variable into six others.
df <- separate(df, "DATA",
               into = c("PoS", "FLEX", "TG", "POL", "ANOT", "REV"),
               sep = ";")

# Remove prefixes from column values.
df <- as.data.frame(sapply(df, function(x) {
    gsub("[[:alnum:][:punct:]]+=", "", x)
}))
df$POL <- as.numeric.factor(df$POL)

# Remove lemmas that give no information about the polarity.
# PS: don't do this if you're going to use the Naive Bayes classifier!
# df <- filter(df, POL != 0)

# Find lemmas whose flexed form can derive from multiple other lemmas.
dupl_i <- duplicated(df$LEM)
dupl_j <- dupl_i | duplicated(df$LEM, fromLast = TRUE)
dupl <- df[dupl_i, ]

# Replace their polarities  by their sum. Then, in the end, average all the
# polarities summed for each lemma (in this step and the previous one).
n <- 0
for (row in seq(nrow(df), 2, -1)) {
    if (dupl_j[row]) {
        if (dupl_i[row]) {
            df[row-1, "POL"] <- df[row-1, "POL"] + df[row, "POL"]
            n <- n + ifelse(mults_ind[row], 2, 1)
        } else {
            df[row, "POL"] <- round(df[row, "POL"] /
                                    (n + ifelse(mults_ind[row], 2, 1)), 2)
            n <- 0
        }
    }
}

# Set the best fit (class) for the polarity values.
# PS: don't do this if you're going to use the voter algorithm!
df$POL <- as.factor(ifelse(df$POL > 0, "positive",
                    ifelse(df$POL < 0, "negative", "neutral")))

# Drop the (now) unnecessary rows and columns.
df <- df %>% filter(!dupl_i) %>% select(LEM, POL)

# Check the resulting dataset.
str(df)
summary(df)
head(df, 10)
tail(df, 10)

# Save the resulting dataset.
write_csv(df, "data/SentiLex-flex-PT02-clean.csv")
