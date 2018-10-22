library(tm)
library(ggraph)
library(wordcloud)
library(tidytext)
library(cluster)
#library(tidyverse)
#library(tidyr)    
#library(igraph)


#1.Create and examine Corpus===============================================================================================
#having changed the directory to where the speeches are located
#define the path of the document collection
cname <- file.path(".", "speech")
cname #check it's done it write
length(dir(cname)) #are the document number as expected?
dir(cname) #what are the names of these documents?

#now, turn this into a Corpus - collection of document
buhariCorpus <- Corpus(DirSource(cname, encoding="UTF-8"))
#check data structure
str(buhariCorpus)
#summary of documents loaded
summary(buhariCorpus)


#examine a corpus - randomly chose one
buhariCorpus[[47]]$content
#this shows some unwanted characters (e.g. <92>) that needs removing and also means all documents need to be accessed

#2.Remove unwanted characters=============================================================================================
buhariCorpus <- tm_map(buhariCorpus, content_transformer(gsub), pattern = "<91>", replacement=" ")
buhariCorpus <- tm_map(buhariCorpus, content_transformer(gsub), pattern = "<92>", replacement=" ")
buhariCorpus <- tm_map(buhariCorpus, content_transformer(gsub), pattern = "<93>", replacement=" ")
buhariCorpus <- tm_map(buhariCorpus, content_transformer(gsub), pattern = "<94>", replacement=" ")
buhariCorpus <- tm_map(buhariCorpus, content_transformer(gsub), pattern = "<95>", replacement=" ")
buhariCorpus <- tm_map(buhariCorpus, content_transformer(gsub), pattern = "<96>", replacement=" ")
buhariCorpus <- tm_map(buhariCorpus, content_transformer(gsub), pattern = "<97>", replacement=" ")
buhariCorpus <- tm_map(buhariCorpus, content_transformer(gsub), pattern = "<U+200B>", replacement=" ")
buhariCorpus <- tm_map(buhariCorpus, content_transformer(gsub), pattern = "<a0>", replacement=" ")
buhariCorpus <- tm_map(buhariCorpus, content_transformer(gsub), pattern = "cross river", replacement="crossriver")
buhariCorpus <- tm_map(buhariCorpus, content_transformer(gsub), pattern = "etc", replacement=" ")
buhariCorpus <- tm_map(buhariCorpus, content_transformer(gsub), pattern = "<e9>", replacement=" ")
buhariCorpus <- tm_map(buhariCorpus, content_transformer(gsub), pattern = "<e3>", replacement=" ")
#3.Further editing=========================================================================================================
#convert lower cases to upper cases
buhariCorpus <- tm_map(buhariCorpus, content_transformer(tolower))
#remove stopwords
buhariCorpus <- tm_map(buhariCorpus, removeWords, stopwords("english"))
#remove punctuations
buhariCorpus <- tm_map(buhariCorpus, removePunctuation)
#remove numbers
buhariCorpus <- tm_map(buhariCorpus, removeNumbers)
#strip white spaces
buhariCorpus <- tm_map(buhariCorpus, stripWhitespace)

#Customised stopwords
customised_stopwords <- c("even", "today","also","now","new", "real", "day", "first", "like")
buhariCorpus <- tm_map(buhariCorpus, removeWords, customised_stopwords)
#4. Create document term matrix====================================================================================================
buhariDTM <- DocumentTermMatrix(buhariCorpus)
buhariDTM #view content
#convert DTM to a normal matrix
buhariTerms <- sort(colSums(as.matrix(buhariDTM)), decreasing=TRUE)
#convert the terms matrix above to a dtaframe for plotting
buhariTermsDF <- data.frame(word=names(buhariTerms), freq=buhariTerms)

#check the occurence of the states in Nigeria in the DTM and how often it occurs
stateDTM <- inspect(buhariDTM[, c("abuja", "lagos", "enugu", "ondo", "anambra", "bauchi", "bayelsa","benue", "borno", 
                                  "delta", "ebonyi", "edo", "ekiti", "gombe", "jigawa", "kaduna", "kano", 
                                  "katsina", "kebbi", "kogi", "kwara", "nasarawa", "niger", "ogun", "osun", "oyo", "plateau",
                                  "rivers", "taraba", "yobe", "zamfara", "adamawa")])
#convert stateDTM to a normal matrix
stateTerms <- colSums(as.matrix(stateDTM))
#convert above to a dataframe for plotting
stateTermsDF <- data.frame(word=names(stateTerms), freq=stateTerms)

#For comparison, get inauguration speech and others into 2 seperate DTM
#1. inaguration matrix
inaugTerms <-  sort(colSums(as.matrix(buhariDTM[1,])), decreasing=TRUE)
#convert the terms matrix above to a dtaframe for plotting
inaugTermsDF <- data.frame(word=names(inaugTerms), freq=inaugTerms)
#2. without inaguration matrix
NoInaugTerms <- sort(colSums(as.matrix(buhariDTM[2:53,])), decreasing=TRUE)
#convert the terms matrix above to a dtaframe for plotting
NoInaugTermsDF <- data.frame(word=names(NoInaugTerms), freq=NoInaugTerms)

#5. Exploratory Analysis====================================================================================================
#a. barchart of occurence of states in the statecorpus
ggplot(subset(stateTermsDF), aes(x = reorder(word, -freq), y = freq)) + 
  geom_bar(stat = "identity") +  
  ggtitle("Top 10 States+Capital Mentioned") + 
  labs(x="States", y="Word frequency") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))

#investigate Ekiti's speeches given that it is the highest occuring state
ekitiTerms <-  sort(colSums(as.matrix(buhariDTM[c(24,11,8),])), decreasing=TRUE)
#convert the terms matrix above to a dataframe for plotting
ekitiTermsDF <- data.frame(word=names(ekitiTerms), freq=ekitiTerms)
#plot as a word cloud
set.seed(2000)    
wordcloud(ekitiTermsDF$word, ekitiTermsDF$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

#b. barchart of word frequency (frequency greater than 50) in the corpus
ggplot(subset(buhariTermsDF, freq>50), aes(x = reorder(word, -freq), y = freq)) + 
  geom_bar(stat = "identity") +  
  ggtitle("Most occuring words: frequency greater than 50") + 
  labs(x="Words", y="Word frequency") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
#plot as word cloud (but frequency greater than 25)
set.seed(2000)    
wordcloud(buhariTermsDF$word, buhariTermsDF$freq, min.freq = 25,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

#c. comparing inaguration speech and others using word cloud
set.seed(12222) 
#par(mfrow=c(1,2)) #places chart side by side
wordcloud(NoInaugTermsDF$word, NoInaugTermsDF$freq, min.freq = 25,
          max.words=100, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
wordcloud(inaugTermsDF$word, inaugTermsDF$freq, min.freq = 25,
          max.words=100, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

#d. occurence of key policy terms
df_buhari <- tidy(buhariDTM)
df_buhari %>% 
  filter(term==c("corruption", "security", "economic")) %>%  
  group_by(document) %>%  
  ggplot(aes(x = reorder(document, -count), y = count)) + 
  geom_bar(stat = "identity") +  
  ggtitle("Occurrences of 3-policy focus in Buhari's speeches") + 
  labs(x="Speech", y="Word frequency") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) 
ggsave('policy.png', width=12, height=10)

#6. Cluster Analysis====================================================================================================
mat <- as.matrix(buhariDTM) #turn DTM into a matrix
docsdissim <- dist(scale(mat)) #calculate distance to create a dissimilarity matrix
h<-hclust(docsdissim, method = 'ward.D') #cluster usisng hiericichal ward.D/ward.D2/complete as the metric 
clustering <- cutree(h, 6)
#"ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
plot(h, cex=0.8, hang=0.1, main = "Cluster Dendogram: Ward", xlab = "Speeches", frame.plot=FALSE)
rect.hclust(h,6, border = "red")

#analysis clusters: e.g. cluster3
cluster3 <- mat[clustering ==3, ]
cluster3TermsDF <- data.frame(word=names(cluster3), freq=cluster3)
wordcloud(cluster3TermsDF$word, cluster3TermsDF$freq, min.freq = 25,
          max.words=100, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

#7. Co-occurence====================================================================================================
some_northern_state <- findAssocs(buhariDTM, c("nasarawa", "kaduna", "kano"), corlimit=0.7)[[1]]
some_northern_state<-cbind(read.table(text =names(some_northern_state),stringsAsFactors = FALSE), some_northern_state)
g<-graph.data.frame(some_northern_state,directed =TRUE)
plot(g)


delta <- findAssocs(buhariDTM, c("niger", "delta"), corlimit=0.78)[[1]]
delta<-cbind(read.table(text =names(delta),stringsAsFactors = FALSE), delta)
g<-graph.data.frame(delta,directed =TRUE)
plot(g)
