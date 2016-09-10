load("./mat/balanced_test.Rdata")
load("./mat/balanced_train.Rdata")
load("./mat/balanced_train_y.Rdata")
load("./mat/balanced_test_y.Rdata")

tweets <- read.csv("./data/used_tweets.csv", stringsAsFactors = FALSE)
tweets$class <- as.factor(tweets$class)
tweets$X <- NULL
cat("\nDim tweets: ", dim(tweets))
cat("\nDim mtrain: ", dim(mtrain))
cat("\nDim mtest: ", dim(mtest))
