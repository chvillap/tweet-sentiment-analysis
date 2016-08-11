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

library(ggplot2)
library(stringr)
library(dplyr)
library(readr)
library(twitteR)
library(Rstem)
library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)

# -----------------------------------------------------------------------------
# Twitter authentication.

consumer_key <- "gqqqh0oogLIKRnzH9YJJ06uLn"
consumer_secret <- "4B8rjy90Bc9Pp1dFDXFNaPGwLfBDoFCBXfy3czZ0iTne1DvwfM"
access_token <- "746869412788117505-BHsrG1AYafuyHWuMN2T5dBsKALV13p5"
access_secret <- "UWs3yzcuTiOwL09EwOvfYg8d4BzpixcQ9QI4H0nGbHr68"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# -----------------------------------------------------------------------------
# Prompt input parameters from user.

validated.prompt <- function(message, pattern = ".*") {
    repeat {
        cat(message)
        # value <- readline() # RStudio
        value <- readLines(con = "stdin", n = 1) # Rscript
        if (grepl(pattern, value))
            break
        else
            print("Wrong format. Please try again.")
    }
    value
}

query <- validated.prompt("Enter the search query (term1+term2+...): ")
lang <- validated.prompt("Enter the language (en|pt): ",
                         pattern = "^(en|pt)$")
since <- validated.prompt("Enter the initial date (YYYY-MM-DD): ",
                          pattern = "^\\d{4}-\\d{2}-\\d{2}$")
until <- validated.prompt("Enter the final date (YYYY-MM-DD): ",
                          pattern = "^\\d{4}-\\d{2}-\\d{2}$")
n <- validated.prompt("Enter the maximum number of tweets: ",
                      pattern = "^[0-9]+$")
n <- as.integer(n)

# -----------------------------------------------------------------------------
# Get the tweets.

cat("Getting tweets...... ")

tweets <- searchTwitter(query, n, lang, since, until)
texts <- sapply(tweets, function(x) { x$getText() })
n <- length(texts)

cat("done!\n")

# -----------------------------------------------------------------------------
# Clean and transform data.

cat("Cleaning and transforming data...... ")

# Remove nongraphical characters, RT, via, and hyperlinks.
texts <- gsub("[^[:graph:]]", " ", texts)
texts <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "\\1", texts)
texts <- gsub("@[[:alnum:][:punct:]]+", "", texts)
texts <- gsub("http[s]?.*\\b", "", texts)

corpus <- Corpus(VectorSource(texts))
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, c("rt", "via",  stopwords(lang)))
# corpus <- tm_map(corpus, stemDocument)

cat("done!\n")

# -----------------------------------------------------------------------------
# Generate a wordcloud with the most frequent words.

cat("Generating a wordcloud...... ")

pdf("wordcloud.pdf")
wordcloud(corpus, max.words = 500, random.color = TRUE, 
          colors = brewer.pal(12, "Set3"), random.order = FALSE,
          scale=c(5, 0.5))
dev.off()

cat("done!\n")

# -----------------------------------------------------------------------------
# Generate a dendrogram grouping frequently associated words.

tdm <- TermDocumentMatrix(corpus)
tdm <- removeSparseTerms(tdm, sparse = 0.97)

tdm_scaled <- scale(tdm)
distances <- dist(tdm_scaled, method = "euclidean")
hclusters <- hclust(distances)

pdf("dendrogram.pdf")
plot(hclusters, xlab = "Terms", ylab = "Similarity")
# cutree(hclusters, k = 5)
# rect.hclust(hclusters, k = 5, border = "green")
dev.off()

# -----------------------------------------------------------------------------
# Sentiment analysis.

if (lang == "en") {
    library(sentiment)

    df_corpus <- data.frame(sapply(corpus, function (x) { x$content }))

    df_polar <- classify_polarity(df_corpus, algorithm = "voter")
    df_polar <- data.frame(TWEET = texts, POLARITY = df_polar[, "BEST_FIT"])

    df_emo <- classify_emotion(df_corpus, algorithm = "bayes")
    df_emo <- data.frame(TWEET = texts, EMOTION = df_emo[, "BEST_FIT"],
                         stringsAsFactors = FALSE)
    df_emo[is.na(df_emo$EMOTION), "EMOTION"] <- "unknown"

} else if (lang == "pt") {
    source("src/classify_polarity.R")
    source("src/classify_emotion.R")

    docs <- sapply(corpus, function(x) { x$content })

    df_polar <- classify_polarity(docs, algorithm = "voter")
    df_polar <- data.frame(TWEET = texts, POLARITY = df_polar$BEST_FIT)

    df_emo <- classify_emotion(docs)
    df_emo <- data.frame(TWEET = texts, EMOTION = df_emo$BEST_FIT,
                         stringsAsFactors = FALSE)
    df_emo[is.na(df_emo$EMOTION), "EMOTION"] <- "unknown"
}

# -----------------------------------------------------------------------------
# Plot some graphics and save the results.

ggsave("polarity.pdf",
    ggplot(df_polar, aes(x = POLARITY)) +
        geom_bar(aes(y = ..count.., fill = POLARITY)) +
        scale_fill_brewer(palette = "Set2") +
        labs(x = "Sentiment polarity", y = "Number of tweets"))

ggsave("emotions.pdf",
    ggplot(df_emo, aes(x = EMOTION)) +
        geom_bar(aes(y = ..count.., fill = EMOTION)) +
        scale_fill_brewer(palette = "Set2") +
        labs(x = "Predominant emotion", y = "Number of tweets"))

write_csv(df_polar, "polarities.csv")
write_csv(df_emo, "emotions.csv")
