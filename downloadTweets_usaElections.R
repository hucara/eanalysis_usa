
# Download classifying and cleaning tweets

rm(list = ls())
source("twitterAuthorization.R")
source("dataRetriever.R")
source("preprocessing.R")
source("corpusBuilding.R")
source("emojiRetriever.R")
num <- 20
mult <- 1
topic <- "#hillary"

# fix defined topic
topic <- paste("+", topic, sep="")

emojiDict <- readEmojiDictionary("./data/emoji_dictionary.csv")
emoteDict <- readEmoticonDictionary("./data/emote_dictionary.csv")

downloadTweets <- function(num, search, class, emojiDict = NULL, emoteDict = NULL)
{
  tweets <- data.frame()
  for(i in 1:length(search))
  {
    message("\nSearch: ", search[i])
    s <- searchTwitter(search[i], n=num, lang = "en")
    message("Transformation...")
    s <- tweetsToDF(s)
    
    if(!is.null(emoteDict))
    {
      message("Dealing with ASCII emotes...")
      emoteWord <- getEmoticonsWord(s$text, emoteDict)
      s$text <- paste(s$text, emoteWord, sep = " ")
    }
    
    message("Parsing hashtags...")
    s$hashtags <- getHashtags(s$text)
    message("Parsin emojis...")
    s$emojis <- getEmojis(s$text)
    
    if(!is.null(emojiDict))
    {
      message("Adding emoji keywords to text...")
      emoKWs <- getEmojiKeywords(s$emojis, emojiDict)
      s$text <- paste(s$text, emoKWs)
    }
    
    message("Cleaning...")
    s$ctext <- cleanTexts(s$text)
    s$chtext <- cleanTexts(s$text, keepHashtags = FALSE)
    s$class <- as.factor(class)
    tweets <- rbind(tweets, s)
  }
  message("Done! Tweets: ", dim(tweets)[1], " variables: ", dim(tweets)[2])
  
  gc()
  return(tweets)
}

#happy
happySyn <- c("#happy", "#joy", "#bliss", "#happiness")
happySyn <- paste(happySyn, topic, sep="")
happy <- data.frame()
repeat
{
  h <- downloadTweets(num, happySyn, "happy", emojiDict, emoteDict)
  happy <- rbind(happy, h)
  happy <- unique(happy)
  
  message("Current #happy unique tweets: ", nrow(happy))
  if(nrow(happy) < num*mult)
  {
    message("Not enough tweets retrieved. Waiting and trying again...")
    Sys.sleep(60)
  }
  else
  {
    rm(h)
    break;
  }
}

#sad
sadSyn <- c("#sad","#unhappy","#depressed", "#bitter", "#heartbroken", "#dismay")
sadSyn <- paste(sadSyn, topic, sep="")
sad <- data.frame()
repeat
{
  s <- downloadTweets(num, sadSyn, "sad", emojiDict, emoteDict)
  sad <- rbind(sad, s)
  sad <- unique(sad)
  
  message("Current #sad unique tweets: ", nrow(sad))
  if(nrow(sad) < num*mult)
  {
    message("Not enough tweets retrieved. Waiting and trying again...")
    Sys.sleep(60)
  }
  else
  {
    rm(s)
    break;
  }
}

#surprised
surprisedSyn <- c("#surprised","#shocked","#amazed", "#astonished", "#omg")
surprisedSyn <- paste(surprisedSyn, topic, sep="")
surprised <- data.frame()
repeat
{
  su <- downloadTweets(num, surprisedSyn, "surprised", emojiDict, emoteDict)
  surprised <- rbind(surprised, su)
  surprised <- unique(surprised)
  
  message("Current #surprised unique tweets: ", nrow(surprised))
  if(nrow(surprised) < num*mult)
  {
    message("Not enough tweets retrieved. Waiting and trying again...")
    Sys.sleep(60)
  }
  else
  {
    rm(su)
    break;
  }
}

#afraid
afraidSyn <- c("#afraid","#scared","#worried", "#fear", "#angst", "#horror")
afraidSyn <- paste(afraidSyn, topic, sep="")
afraid <- data.frame()
repeat
{
  af <- downloadTweets(num, afraidSyn, "afraid", emojiDict, emoteDict)
  afraid <- rbind(afraid, af)
  afraid <- unique(afraid)
  
  message("Current #afraid unique tweets: ", nrow(afraid))
  if(nrow(afraid) < num*mult)
  {
    message("Not enough tweets retrieved. Waiting and trying again...")
    Sys.sleep(60)
  }
  else
  {
    rm(af)
    break;
  }
}

#angry
angrySyn <- c("#angry","#mad","#furious", "#annoyed", "#rage", "#jealous", "#jelly", "#frustrated", "#outrage", "#grumpy", "#anger")
angrySyn <- paste(angrySyn, topic, sep="")
angry <- data.frame()
repeat
{
  an <- downloadTweets(num, angrySyn, "angry", emojiDict, emoteDict)
  angry <- rbind(angry, an)
  angry <- unique(angry)
  
  message("Current #angry unique tweets: ", nrow(angry))
  if(nrow(angry) < num*mult)
  {
    message("Not enough tweets retrieved. Waiting and trying again...")
    Sys.sleep(60)
  }
  else
  {
    rm(an)
    break;
  }
}

#disgusted
disgustedSyn <- c("#disgusted","#sickened","#offended", "#sick", "#weary", "#wtf")
disgustedSyn <- paste(disgustedSyn, topic, sep="")
disgusted <- data.frame()
repeat
{
  di <- downloadTweets(num, disgustedSyn, "disgusted", emojiDict, emoteDict)
  disgusted <- rbind(disgusted, di)
  disgusted <- unique(disgusted)
  
  message("Current #disgusted unique tweets: ", nrow(disgusted))
  if(nrow(disgusted) < num*mult)
  {
    message("Not enough tweets retrieved. Waiting and trying again...")
    Sys.sleep(60)
  }
  else
  {
    rm(di)
    break;
  }
}

tweets <- rbind(happy, sad, surprised, afraid, angry, disgusted)
write.csv(tweets, "./data/tweets_usaElections_hillary.csv")

# write.csv(happy, "./data/medium/happy.csv")
# write.csv(sad, "./data/medium/sad.csv")
# write.csv(surprised, "./data/medium/surprised.csv")
# write.csv(afraid, "./data/medium/afraid.csv")
# write.csv(angry, "./data/medium/angry.csv")
# write.csv(disgusted, "./data/medium/disgusted.csv")
