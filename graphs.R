# graphs
set.seed(1234)
library("wordcloud")
library("RColorBrewer")

folder <- "./graph_usaElection/"

# WORDCLOUDS #
# General
v <- sort(colSums(mtrain), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
head(d, 10)

png(file=paste(folder,"wc_general.png",sep = "/"), width=500, height=500, res = 120)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=150, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(7, "Dark2"))
dev.off()

# happy
v <- sort(colSums(mtrain[which(train.y == "sad"),]), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
head(d, 10)

png(file=paste(folder,"wc_sad.png",sep = "/"), width=500, height=500, res = 120)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=150, random.order=FALSE, rot.per=0.35,
          colors = brewer.pal(9, "Greys")
          )
dev.off()

# sad
v <- sort(colSums(mtrain[which(tweets$class == "angry"),]), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
head(d, 10)

png(file=paste(folder,"wc_angry.png",sep = "/"), width=500, height=500, res = 120)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=150, random.order=FALSE, rot.per=0.35,
          color="Orange")
dev.off()


# ZIPFS LAW #
png(file=paste(folder,"freq_zipfs_law.png",sep = "/"), width=500, height=500, res = 120)
ggplot(data=d[d$freq > 0,], aes(y=d$freq[d$freq > 0], x=1:length(d$freq[d$freq > 0]))) + 
  labs(x = "Num", y = "Freq", title = "") +
  geom_point() + 
  geom_line(color = "Red")
dev.off()

# ZIPFS LAW 2 #
png(file=paste(folder,"freq_word.png",sep = "/"), width=500, height=500, res = 120)
ggplot(data=head(d, 30), aes(y=head(d$word,30), x=head(d$freq, 30))) + 
  labs(x = "Num", y = "Freq", title = "") +
  geom_point() + 
  geom_line(color = "Red")
dev.off()

# CLASS BALANCE #
uni <- tweets[!duplicated(tweets$cwtext),]
pie(prop.table(table(uni$class)))

png(file=paste(folder,"pie_class_prop.png",sep = "/"), width=500, height=450, res = 120)
ggplot(uni, aes(x=factor(1), fill=class))+
  geom_bar(width = 1) +
  coord_polar("y") + 
  labs(x = "", y = "", title = "") + 
  scale_fill_brewer(palette="Dark2") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())
dev.off()

count <- data.frame(table(uni$class))
png(file=paste(folder,"bar_class_prop.png",sep = "/"), width=500, height=500, res = 120)
ggplot(count, aes(x=Var1, y = Freq, fill=Var1)) +
  geom_bar(stat = "identity") + 
  labs(x = "", y = "Count", title = "", fill = "") +
  geom_text(aes(label=round(Freq,4)), vjust=1.5, color='white', position=position_dodge(.9), size=3) +
  scale_fill_brewer(palette="Dark2") +
  theme(axis.text.x = element_blank())
dev.off()

# MODEL RESULTS GRAPH
load("./models/svm_mauc.Rdata")
load("./models/rf_mauc.Rdata")
load("./models/xgb_mauc.Rdata")
load("./models/nb_mauc.Rdata")
load("./models/nn_mauc.Rdata")

load("./models/svm_conf.Rdata")
load("./models/rf_conf.Rdata")
load("./models/xgb_conf.Rdata")
load("./models/nb_conf.Rdata")
load("./models/nn_conf.Rdata")

maucs <- c(svm_mauc$auc, rf_mauc$auc, xgb_mauc$auc, nb_mauc$auc, nn_mauc$auc)
accus <- c(svm_sum[["Mean_Balanced_Accuracy"]], rf_sum[["Mean_Balanced_Accuracy"]], xgb_sum[["Mean_Balanced_Accuracy"]], nb_sum[["Mean_Balanced_Accuracy"]], nn_sum[["Mean_Balanced_Accuracy"]])

#accus <- c(svm_conf$overall[[1]], rf_conf$overall[[1]], xgb_conf$overall[[1]], nb_conf$overall[[1]], nn_conf$overall[[1]])
kapps <- c(svm_conf$overall[[2]], rf_conf$overall[[2]], xgb_conf$overall[[2]], nb_conf$overall[[2]], nn_conf$overall[[2]])
names <- c("SVM", "RF", "XGB", "NBayes", "NNet")

results <- data.frame(Accuracy = accus, mAUC = maucs, Kappa = kapps, Names = names)

png(file=paste(folder,"bar_models_accuracy.png",sep = "/"), width=500, height=500, res = 120)
ggplot(results, aes(x = Names, y = Accuracy, fill = Names)) + 
  geom_bar(position='dodge', stat='identity') + 
  labs(x = "", y = "Accuracy", title = "", fill = "") +
  geom_text(aes(label=round(Accuracy,4)), vjust=1.5, color='white', position=position_dodge(.9), size=3) +
  scale_fill_brewer(palette="Dark2")
dev.off()

png(file=paste(folder,"bar_models_mauc.png",sep = "/"), width=500, height=500, res = 120)
ggplot(results, aes(x = Names, y = mAUC, fill = Names)) + 
  geom_bar(position='dodge', stat='identity') + 
  labs(x = "", y = "mAUC", title = "", fill = "") +
  geom_text(aes(label=round(mAUC,4)), vjust=1.5, color='white', position=position_dodge(.9), size=3) +
  scale_fill_brewer(palette="Dark2")
dev.off()

png(file=paste(folder,"bar_models_kappa.png",sep = "/"), width=500, height=500, res = 120)
ggplot(results, aes(x = Names, y = Kappa, fill = Names)) + 
  geom_bar(position='dodge', stat='identity') + 
  labs(x = "", y = "Kappa", title = "Kappa en #Rio2016", fill = "") +
  geom_text(aes(label=round(Kappa,4)), vjust=1.5, color='white', position=position_dodge(.9), size=3) +
  scale_fill_brewer(palette="Dark2")
dev.off()
