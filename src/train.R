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

source("src/nbayes.R")

# Our training data come from the SentiLex-PT02 lexicon (after cleaning it up).
# It contains more than 44k words, associated with their respective polarities.
df_train <- read.csv("data/SentiLex-flex-PT02-clean-nbayes.csv")

# The prior probabilities are set this way because we want to prioritize
# neutral predictions when the true polarity can"t be defined due to the
# lack of any positive/negative word in the test data.
priori <- c(positive = 0.33, negative = 0.33, neutral = 0.34)

# Train the model and save it for future use.
model <- nbayes.train(df_train$LEM, df_train$POL, priori)
saveRDS(model, "model/nbayes.rds")
