# corpus building

createCleanCorpus <- function(texts, stopwords, stemming = TRUE)
{
  if(require(tm) && require(twitteR) && require(SnowballC) && length(texts)>0)
  {
    # input must be a vectorized corpus
    corpus <- Corpus(VectorSource(texts))
    message("Transforming to lower case...")
    corpus <- tm_map(corpus, content_transformer(tolower))
    message("Removing punctuation...")
    corpus <- tm_map(corpus, removePunctuation)
    if(!is.null(stopwords))
    {
      message("Removing stopwords...")
      corpus <- tm_map(corpus, removeWords, stopwords)
    }
    if(stemming == TRUE)
    {
      message("Stemming document...")
      corpus <- tm_map(corpus, stemDocument, language="en")
    }
    if(!is.null(stopwords))
    {
      message("Removing stopwords...")
      corpus <- tm_map(corpus, removeWords, stopwords)
    }
    message("Cleaning whitespaces...")
    corpus <- tm_map(corpus, stripWhitespace)
    message("Transforming to lower case...")
    corpus <- tm_map(corpus, content_transformer(tolower))
    message("Done!")
    
    return(corpus)
  }
}