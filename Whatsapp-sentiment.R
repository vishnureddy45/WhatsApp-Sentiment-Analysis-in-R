####################################################################################

#Collecting and Analyzing whatsapp Data using R

#####################################################################################

#command to set the directory
#setwd("specify the path to the desired folder")


whatsapp_analysis<-function(filename){

#Required libraries to load 
library(ggplot2)
library(RWeka)
library(Scale)
library(reshape2)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(stringr)
library(syuzhet) 
library(dplyr)
library(lubridate)

  
#get the data from whatsapp chat 
text <- readLines(filename)
text<-as.matrix(text)


##########################Data Cleaning#############################################
text<-tolower(text)
cleaned<-gsub(" *n't | no | *n't " , " not " ,text)
cleaned<-gsub("â???T","",text)
#corpus creation
cleaned <- VCorpus(VectorSource(text))
cleaned <- tm_map(cleaned, removeNumbers)
cleaned <- tm_map(cleaned, removeWords, stopwords("english"))
cleaned <- tm_map(cleaned, removeWords, c("vishnu","vish"))
cleaned <- tm_map(cleaned, removePunctuation)
cleaned <- tm_map(cleaned, stripWhitespace)
#Function to create Bigram
# BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min =2 , max = 2))
# TDM <- TermDocumentMatrix(cleaned,control = list(tokenize = BigramTokenizer))

TDM <- TermDocumentMatrix(cleaned)
TDM <- as.matrix(TDM)
v <- sort(rowSums(TDM),decreasing=TRUE)

#Data frame
data <- data.frame(word = names(v),freq=v)
head(data, 10)

#############Wordcloud Generation#################################
set.seed(1234)
pdf("whatsapp_wordcloud.pdf",height = 8, width = 13)
wordcloud(words = data$word, freq = data$freq, min.freq = 1,max.words=700, random.order=FALSE, rot.per=0.35,
colors=brewer.pal(8, "Set2"))
dev.off()

##fetch sentiment words from texts
Sentiment <- get_nrc_sentiment(text)
head(Sentiment)
text <- cbind(text,Sentiment)

##To count the sentiment words by category
Total_Sentiment <- data.frame(colSums(text[,c(2:11)]))
names(Total_Sentiment) <- "count"
Total_Sentiment <- cbind("sentiment" = rownames(Total_Sentiment), Total_Sentiment)
rownames(Total_Sentiment) <- NULL

##Total sentiment score of all texts
ggplot(data = Total_Sentiment, aes(x = sentiment, y = count)) +
geom_bar(aes(fill = sentiment), stat = "identity") +
theme(legend.position = "none") +
xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score")
ggsave("Total_Sentiment_score.pdf",height = 8, width = 13)

}

##################Function to run the total code###################################
#Place the chat file in the working directory 
#To check the current directory
#getwd()

whatsapp_analysis(filename = "chat.txt")

####################################################################################
