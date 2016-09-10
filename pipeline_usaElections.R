require(caret)
require(tm)
source("dataRetriever.R")
source("preprocessing.R")
source("corpusBuilding.R")
source("stopWords.R")
set.seed(1234)

# Read the data
tweets <- read.csv("./data/tweets_election2016.csv", stringsAsFactors = FALSE)
tweets$class <- as.factor(tweets$class)
tweets$X <- NULL

# tweets$cwtext <- getTextsWithoutClassHashtag(tweets)
# tweets$cwtext <- cleanTexts(tweets$cwtext)
# write.csv(tweets, file = "./data/tweets_election2016.csv")

dim(tweets)

prop.table(table(tweets$class))
table(tweets$class)

# create partitions with equivalent class sample ratios
inTrain <- createDataPartition(y=tweets[,"class"], p = .80, list = FALSE)
train <- createCleanCorpus(tweets$cwtext[inTrain], sw, F)
test <- createCleanCorpus(tweets$cwtext[-inTrain], sw, F)

prop.table(table(tweets$class[inTrain]))
length(test)
length(train)

# create dtms to build an intersected dictionary with them
# train.dtm <- DocumentTermMatrix(train, control = list(wordLengths = c(3, 10)))
# test.dtm <- DocumentTermMatrix(test, control = list(wordLengths = c(3, 10)))
train.dtm <- DocumentTermMatrix(train, control = list(wordLengths = c(3, 10)))
test.dtm <- DocumentTermMatrix(test, control = list(wordLengths = c(3, 10)))
train.dtm
test.dtm

#train.d <- findFreqTerms(train.dtm, lowfreq = 7)
train.d <- findFreqTerms(train.dtm, lowfreq = 1)
test.d <- findFreqTerms(test.dtm, lowfreq = 1)

dict <- intersect(train.d, test.d)
length(dict)
head(dict)

# create the real dtm
message("Weighting = TERM FREQUENCY")
# train.dtm <- DocumentTermMatrix(train, control = list(dictionary=dict, weighting = tm::weightTfIdf))
# test.dtm <- DocumentTermMatrix(test, control = list(dictionary=dict, weighting = tm::weightTfIdf))
train.dtm <- DocumentTermMatrix(train, control = list(dictionary=dict, weighting = tm::weightTf))
test.dtm <- DocumentTermMatrix(test, control = list(dictionary=dict, weighting = tm::weightTf))
train.dtm <- DocumentTermMatrix(train, control = list(dictionary=dict))
test.dtm <- DocumentTermMatrix(test, control = list(dictionary=dict))

# remove empty documents due to the intersect dictionary
rowTotalsTrain <- apply(train.dtm, 1, sum)
train.dtm   <- train.dtm[rowTotalsTrain> 0,]
rowTotalsTest <- apply(test.dtm, 1, sum)
test.dtm   <- test.dtm[rowTotalsTest> 0,] 

# we also have to adjust the class values for the documents
train.y <- tweets$class[inTrain]
train.y <- train.y[which(rowTotalsTrain > 0)]

test.y <- tweets$class[-inTrain]
test.y <- test.y[which(rowTotalsTest > 0)]

# conversion to matrix
mtrain <- as.matrix(train.dtm)
mtest <- as.matrix(test.dtm)

# save(mtrain, file = "./mat/mtrain_usaElection.Rdata")
# save(mtest, file = "./mat/mtest_usaElection.Rdata")
# save(train.y, file = "./mat/train_y_usaElection.Rdata")
# save(test.y, file = "./mat/test_y_usaElection.Rdata")
# save(train.dtm, file = "./mat/train_dtm_usaElection.Rdata")
# save(test.dtm, file = "./mat/test_dtm_usaElection.Rdata")

# clean a bit of memory
rm(train.dtm)
rm(test.dtm)
rm(ctweets)
gc()
