library(quanteda)
library(pROC)
library(rpart)
library(rpart.plot)
library(caret)

pre20text <- read.csv('pre20.csv',stringsAsFactors = F)
str(pre20text)


#delete  unwanted columns
cols.dont.want <- c("reviewer_name", "date", "listing_id")
pre20text <- pre20text[, !names(pre20text) %in% cols.dont.want, drop = F]

#create corpus
pre20Corpus <- corpus(pre20text$comments)

# Create a dfm (UNITGRAM) data future matrix
# Remove stop words and perform stemming
library(stopwords)
pre20Dfm  <- dfm(pre20Corpus,
               remove_punc = T,
               remove = c(stopwords("english")),
               stem = T)
dim(pre20Dfm)
# topfeatures(pre20Dfm,50)


#remove unwanted words or symbols in this case
stopwords1 <-c("us", "like", "just","can", "day", "area", "get", "need","also", "definit", "use", "well", "great", "place","stay","day", "everyth", "need", "get", "de", "us","just")

pre20Dfm <- dfm_remove(pre20Dfm,stopwords1)
dim(pre20Dfm)
# topfeatures(pre20Dfm,30)

# #remove infrequent terms #significantly reduced dimentionality
# pre20Dfm<- dfm_trim(pre20Dfm,min_termfreq=6, min_docfreq=2)
# dim(pre20Dfm)

pre20Dfm <- dfm_trim(pre20Dfm,min_docfreq=2000, verbose=TRUE)
dim(pre20Dfm)

rowTotals <- apply(pre20Dfm, 1, sum) #Find the sum of words in each Document
pre20Dfm <- pre20Dfm[rowTotals> 0, ]           #remove all docs without words
head(pre20Dfm)
dim(pre20Dfm)


#frequency analysis
pretstat_freq <- textstat_frequency(pre20Dfm)
head(pretstat_freq, 20)

#WORDCLOUD
textplot_wordcloud(pre20Dfm,max_words=100)


#most frequent tersm
library(ggplot2)
pre20Dfm %>% 
  textstat_frequency(n = 20) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()

#clean
term_clean <- textstat_simil(pre20Dfm,
                                selection="clean",
                                margin="feature",
                                method="correlation")
as.list(term_clean,n=20)


#---topic modeling pre20
library(topicmodels)
library(tidytext)

#summary(pre20Dfm,na.strings=c("NA",""))
#str(pre20Dfm)


#get error about 0 rows/ will delete these
row.sum=apply(pre20Dfm,1,FUN=sum) #sum by raw each raw of the table
pre20Dfm=pre20Dfm[row.sum!=0,]
head(pre20Dfm)

#explore LDA_VEM topic model with 3 topics
preLda <- LDA(pre20Dfm,k=3,control=list(seed=50)) 
preLda
#head(pre20Dfm)

# Term-topic probabilities
preLDA_td <- tidy(preLda)
preLDA_td

# Visulize most common terms in each topic
library(ggplot2)
library(dplyr)

top_terms <- preLDA_td %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


# View topic 8 terms in each topic
preLDA_td<-as.matrix(terms(preLDA_td,8))
View(preLDA_td)



#############################################################################
#POST 2020

post20text <- read.csv('post20.csv',stringsAsFactors = F)
str(pre20text)


#delete  unwanted columns
cols.dont.want <- c("reviewer_name", "date", "listing_id")
post20text <- post20text[, !names(post20text) %in% cols.dont.want, drop = F]


#create corpus
post20Corpus <- corpus(post20text$comments)
summary(post20Corpus)

# Create a dfm (UNITGRAM) data future matrix
post20Dfm <- dfm(post20Corpus)
topfeatures(post20Dfm)

# Remove stop words and perform stemming
library(stopwords)
post20Dfm  <- dfm(post20Corpus,
                 remove_punc = T,
                 remove = c(stopwords("english")),
                 stem = T)
dim(post20Dfm)
# topfeatures(pre20Dfm,50)


#remove unwanted words or symbols in this case
#stopwords1 <-c("great", "place","stay","day", "everyth", "need", "get", "de", "us","just")

post20Dfm <- dfm_remove(post20Dfm,stopwords1)
dim(post20Dfm)
# topfeatures(pre20Dfm,30)

#remove infrequent terms #significantly reduced dimentionality
post20Dfm<- dfm_trim(post20Dfm,min_termfreq=6, min_docfreq=2)
dim(post20Dfm)

post20Dfm <- dfm_trim(post20Dfm,min_docfreq=50, verbose=TRUE)
dim(post20Dfm)

rowTotals <- apply(post20Dfm, 1, sum) #Find the sum of words in each Document
post20Dfm <- post20Dfm[rowTotals> 0, ]           #remove all docs without words
head(post20Dfm)
dim(post20Dfm)



#frequency analysis
poststat_freq <- textstat_frequency(post20Dfm)
head(poststat_freq, 20)

#WORDCLOUD
textplot_wordcloud(post20Dfm,max_words=100)


#most frequent tersm
library(ggplot2)
post20Dfm %>% 
  textstat_frequency(n = 20) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()


#clean
term_cleanpost <- textstat_simil(post20Dfm,
                             selection="clean",
                             margin="feature",
                             method="correlation")
as.list(term_cleanpost,n=20)



#---topic modeling post20
library(topicmodels)
library(tidytext)


#get error about 0 rows/ will delete these
row.sum=apply(post20Dfm,1,FUN=sum) #sum by raw each raw of the table
post20Dfm=post20Dfm[row.sum!=0,]

head(post20Dfm)



#write.csv(pre20Dfm,'pre20dfminprogress.csv')


#explore LDA_VEM topic model with 4 topics
postLda <- LDA(post20Dfm,k=3,control=list(seed=50)) 
postLda
#head(pre20Dfm)


# Term-topic probabilities
postLDA_td <- tidy(postLda)
postLDA_td


# Visulize most common terms in each topic
#library(ggplot2)
#library(dplyr)

top_terms <- postLDA_td %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()



# View topic 8 terms in each topic
postLDA_td<-as.matrix(terms(postLDA_td,15))
View(postLDA_td)

