setwd("D:\\TestAnalytical\\Twitter")

install.packages("twitteR")
install.packages("ROAuth")
install.packages("RCurl")
install.packages("tm")
install.packages("corpus")
install.packages("reshape")
install.packages("plyr")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("devtools")
install.packages("caret")
install.packages("tau")
install.packages("tokenizers")
install.packages("stopwords")
install.packages("dplyr")
install.packages("textclean")
install.packages("shinyWidgets")

devtools::install_github("nurandi/katadasaR")

#Download sertifikat dari curl
download.file(url = "http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")

psbbtwt = searchTwitter("psbb -filter:retweets", n=2000, lang="id")
df_twt <- do.call("rbind", lapply(psbbtwt, as.data.frame))
write.csv(df_twt, file="D:/TestAnalytical/psbb.csv", row.names = F)

tweets <- read.csv(file="D:/TestAnalytical/psbb.csv")
tweets <- tweets$text %>%
  as.character()
head(tweets)

tweets[1]

#text subbing
testtweets <- gsub( "\n"," ",testtweets)

#hastag replacement
testtweets <- replace_html(testtweets) # replace html with blank 
testtweets <- replace_url(tweets)   # replace URLs with blank
testtweets <- replace_emoji(testtweets)
testtweets <- replace_tag(testtweets, pattern = "@([A-Za-z0-9_]+)",replacement="") # remove mentions
testtweets <- replace_hash(testtweets, pattern = "#([A-Za-z0-9_]+)",replacement="")
testtweets <- replace_tag(testtweets)
testtweets[5]

testtweets <- testtweets %>% 
  replace_emoji(.) %>% 
  replace_html(.)

testtweets <- tweets %>% 
  replace_html() %>% # replace html with blank 
  replace_url()   # replace URLs with blank

testtweets <- testtweets %>% 
  replace_tag(tweets, pattern = "@([A-Za-z0-9_]+)",replacement="") %>%  # remove mentions
  replace_hash(tweets, pattern = "#([A-Za-z0-9_]+)",replacement="")      # remove hashtags

# import Kamus Slangword
spell.lex <- read.csv("D:/TestAnalytical/slangword.csv")

# replace internet slang
testslang <- replace_internet_slang(testtweets, slang = paste0("\\b",
                                                               spell.lex$slang, "\\b"),
                                    replacement = spell.lex$formal, ignore.case = TRUE)
write.csv(testslang, file="D:/TestAnalytical/slangfinal.csv", row.names = F)
slangfinal <- read.csv(file="D:/TestAnalytical/slangfix.csv")
slangfinal <- slangfinal$text %>%
  as.character()
head(slangfinal)
slangfinal[5]

#text stripping
testtweets <- strip(testtweets)

#to see duplicate data
testtweets <- testtweets 
as.data.frame() 
distinct()

# number of tweet rows after duplicated text removed
nrow(tweets)

katadasaR("mari")


stemming <- function(x){
  paste(lapply(x,katadasaR),collapse = " ")}

teststem <- lapply(tokenize_words(teststem[]), stemming)
teststem[1]
teststem <- strip(teststem)
teststem <- stripWhitespace(teststem)
write.csv(teststem, file="D:/TestAnalytical/stemfinal.csv", row.names = F)

require(katadasaR)
word.list = str_split(prefix, '\\s+')
words = unlist(word.list)
words = sapply(words, katadasaR)

library(tokenizers)
testtoken <- tokenize_words(teststem)
head(testtoken,3)
testtoken[2]
write.csv(testtoken, file="D:/TestAnalytical/tokenfinal.csv", row.names = F)

#Remove stopwords
myStopwords <- readLines("D:/TestAnalytical/stopword.txt")
teststopwords <- as.character(teststem)
teststopwords <- tokenize_words(teststopwords, stopwords = myStopwords)

nbc<-read.csv("D:/TestAnalytical/AnalisisSentimen/nbc.csv")
str(nbc)
View(nbc)

table(nbc$Sentimen)
prop.table(table(nbc$Sentimen))
nbc$text <- as.character(nbc$Text)
nbc$textlength <- nchar(nbc$Text)

#Data Latih Semua Data
psbb_train <- nbc[1:700,]
psbb_train

#Data Latih Sentimen
nbc_train_labels <- nbc[1:700,]$Sentimen
nbc_train_labels
table(nbc_train_labels)
prop.table(table(nbc_train_labels))

#Data Uji Semua Data
psbb_test <- nbc[701:1000,]
psbb_test

#Data Uji Sentimen
nbc_test_labels <- nbc[701:1000,]$Sentimen
nbc_test_labels
table(nbc_test_labels)
prop.table(table(nbc_test_labels))

#DTM All Data
nbc_corpus <- VCorpus(VectorSource(nbc$Sistem))
psbbdtm <- DocumentTermMatrix(nbc_corpus)
psbb_train_dtm <- psbbdtm[1:700,]
psbb_test_dtm <- psbbdtm[701:1000,]
inspect(psbbdtm[1:5, 1:5])
inspect(psbbdtm)

install.packages('e1071', dependencies=TRUE)

install.packages('caret', dependencies=TRUE)

#Freq Word
frequent_words <- findFreqTerms(psbb_train_dtm, 5)
frequent_words
psbb_freq_word_train <- psbb_train_dtm[, frequent_words]
psbb_freq_word_train
psbb_freq_word_test <- psbb_test_dtm[, frequent_words]
psbb_freq_word_test
length(frequent_words)
frequent_words2 <- findFreqTerms(psbb_test_dtm, )


#Yes No Word
yes_or_no <- function(x){
  y <- ifelse(x > 0,1,0)
  y <- factor(y, levels = c(0,1),
              labels = c("No","Yes"))
  y
}
psbb_train <- apply(psbb_freq_word_train, 2, yes_or_no)
data.frame(psbb_train)
psbb_test <- apply(psbb_freq_word_test, 2, yes_or_no)
data.frame(psbb_test)

#Pelatihan
library(e1071)
psbb_classifier <- naiveBayes(psbb_train, nbc_train_labels, laplace = 1)
system.time(psbb_classifier <- naiveBayes(psbb_train, nbc_train_labels, laplace = 1))
psbb_classifier
class(psbb_classifier)
length(nbc_classifier)
table("Predictions"=psbb_test_pred, "actual"=nbc_classifier)
confusionMatrix(table(nbc_test_pred, nbc_classifier))

#Pengujian
psbb_test_pred <- predict(psbb_classifier, newdata = psbb_test)
system.time(psbb_test_pred <- predict(psbb_classifier, newdata = psbb_test))
psbb_test_pred
table(psbb_test_pred)
table(psbb_test_pred, nbc_test_labels)
prop.table(table(psbb_test_pred, nbc_test_labels))

psbbmodel <- train(Sentimen ~ ., data=nbc, method = "naive_bayes")
studentTestPred <- predict(psbbmodel, test)
confusionMatrix(studentTestPred, test$Sentimen)

#Histogram
histogram_negative <- which(nbc$Sentimen == "negatif")
histogram_positive <- which(nbc$Sentimen =="positif")
histogram_netral <- which(nbc$Sentimen =="netral")

hist1 <- hist(histogram_positive, main = "Histogram Data Sentimen Positif", col ="green")
hist2 <- hist(histogram_netral, main = "Histogram Data Sentimen Netral", col ="blue")
hist3 <- hist(histogram_negative, main = "Histogram Data Sentimen Negatif", col ="red")

#Pie Chart
mytable <- table(nbc$Sentimen)
lbls <- paste(names(mytable), "\n", mytable, sep = "")
pie(mytable, labels = lbls, main = "Diagram Pie dari Data Sentimen \n (Data Positif, Netral, dan Negatif)")
mytable2 <- table(nbc_train_labels)
lbls <- paste(names(mytable2), "\n", mytable2, sep = "")
pie(mytable2, labels = lbls, main = "Diagram Pie Data Latih")
mytable3 <- table(nbc_test_labels)
lbls <- paste(names(mytable3), "\n", mytable3, sep = "")
pie(mytable3, labels = lbls, main = "Diagram Pie Data Uji")

#Wordcloud
negatif_cloud <- which(nbc$Sistem == "negatif")
netral_cloud <- which(nbc$Sistem == "netral")
positif_cloud <- which(nbc$Sistem == "positif")
require(wordcloud)
require(RColorBrewer)
wordcloud(words = nbc_corpus[negatif_cloud],
          min.freq = 1, max.words = 100, random.order = FALSE,
          rot.per = 0.35, colors = brewer.pal(8, "Dark2"),
          main = "Wordcloud Negatif")
wordcloud(words = nbc_corpus[positif_cloud],
          min.freq = 1, max.words = 100, random.order = FALSE,
          rot.per = 0.35, colors = brewer.pal(8, "Dark2"),
          main = "Wordcloud Positif")
wordcloud(words = nbc_corpus[netral_cloud],
          min.freq = 1, max.words = 100, random.order = FALSE,
          rot.per = 0.35, colors = brewer.pal(8, "Dark2"),
          main = "Wordcloud Netral")


#Give score to sentiment
score.sentiment = function(sentences, words.positive, words.negative, .progress = 'none'){
  require(plyr)
  require(stringr)
  require(katadasaR)
  list = laply(sentences, function(sentence, words.positive, words.negative){
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    pos.matches =  match(words, words.positive)
    neg.matches = match(words, words.negative)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    pp = sum(pos.matches)
    nn = sum(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    list1=c(score, pp, nn)
    return(list1)
  }, words.positive, words.negative, .progress = .progress)
  score_new = laply(list, '[[',1)
  pp1 = score = laply(list, '[[', 2)
  nn1 = score = laply(list, '[[', 3)
  scores.df = data.frame(score = score_new, text = sentences)
  positive.df = data.frame(Positive = pp1, text = sentences)
  negative.df = data.frame(Negative = nn1, text = sentences)
  list_df = list(scores.df, positive.df, negative.df)
  return(scores.df)
}

#Lexicon
sentences <- read.csv("D:/TestAnalytical/stopfinal.csv", header = TRUE)
str(sentences)
pos.words = scan("D:/TestAnalytical/positive.txt", what = "character", comment.char = ";")
neg.words = scan("D:/TestAnalytical/negative.txt", what = "character", comment.char = ";")
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{ 
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    words = sapply(words, katadasaR)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(Score=scores, Text=sentences)
  return(scores.df)
}
hasil = score.sentiment(sentences$Text, pos.words, neg.words)
View(hasil)
hasil$Sentimen<- ifelse(hasil$Score<0, "negatif",ifelse(hasil$Score==0,"netral","positif"))
hasil$Sentimen
View(hasil)
data <- hasil[c(2,1,3)]
View(data)
write.csv(data, file="D:/TestAnalytical/labeldata.csv")
