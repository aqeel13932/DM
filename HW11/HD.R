rm(list=ls())
setwd('/home/aqeel/Study/DM/CourseProject/')
# Word cloud code adopted from http://www.r-bloggers.com/word-clouds-using-text-mining/
#install.packages("tm")
library(readr)
library(sqldf)
library(wordcloud)
library(tm)

#cat("Reading data\n")
train <- read_csv('Search Relatives/df_train_stemmed.csv')
test <- read_csv('Search Relatives/df_test_stemmed.csv')
desc <- read_csv('Search Relatives/df_pro_desc_stemmed.csv')
attr <- read_csv('Search Relatives/df_attr_stemmed.csv')
n_words <- function(keyword){
  return(length(unlist(strsplit(keyword,"\\S+"))))
}

get_word_frequency <- function(vec){
  trainCorpus <- Corpus(VectorSource(vec))
  trainCorpus = tm_map(trainCorpus, content_transformer(tolower))
  trainCorpus = tm_map(trainCorpus, removePunctuation)
  trainCorpus = tm_map(trainCorpus, removeWords, stopwords("english"))
  dtm_matrix = TermDocumentMatrix(trainCorpus, control = list(minWordLength = 1))
  m = as.matrix(dtm_matrix)
  v = sort(rowSums(m), decreasing = TRUE)
  return(v)
}


## Exploring search terms
st_train <- unique(train$search_term)
st_test <- unique(test$search_term)
diff_terms_1 <- setdiff(st_train,st_test)
diff_terms_2 <- setdiff(st_test,st_train)
common_terms <- intersect(st_train,st_test)

(paste("Number of search terms in train :",(length(st_train)),sep=" "))
(paste("Number of search terms in test :",(length(st_test)),sep=" "))
print(paste("Number of terms in train not in test :",(length(diff_terms_1)),sep=" "))
print(paste("Number of terms in test not in train :",(length(diff_terms_2)),sep=" "))
print(paste("Number of common terms in test and train :",(length(common_terms)),sep=" "))


train_term_words <- sapply(st_train,n_words)
cat('Number of words in train terms\n')
print(table(train_term_words))
hist(train_term_words,main="Number of words in train search terms", xlab="n_words",ylab="Frequency",col="skyblue1")

test_term_words <- sapply(st_test,n_words)
cat('Number of words in test terms\n')
print(table(test_term_words))
hist(test_term_words,main="Number of words in test search terms", xlab="n_words",ylab="Frequency",col="skyblue1")



cat('Words in train terms\n')
train_term_frequency <- get_word_frequency(st_train)
set.seed(1)
wordcloud(names(train_term_frequency), train_term_frequency, min.freq = 60)

#cat('Words in test terms\n')
test_term_frequency <- get_word_frequency(st_test)
set.seed(1)
wordcloud(names(test_term_frequency), test_term_frequency, min.freq = 60)

## Exploring product data
train_pid <- unique(train$product_uid)
test_pid <- unique(test$product_uid)
common_pid <- intersect(train_pid,test_pid)
unique_train_pid <- setdiff(train_pid,test_pid)
unique_test_pid <- setdiff(test_pid,train_pid)

print(paste("Number of unique product ids in train :",(length(train_pid)),sep=" "))
print(paste("Number of unique product ids in test :",(length(test_pid)),sep=" "))
print(paste("Number of common product ids :",(length(common_pid)),sep=" "))
print(paste("Number of pid in train not in test :",(length(unique_train_pid)),sep=" "))
print(paste("Number of pid in test not in train :",(length(unique_test_pid)),sep=" "))

## Exploring product description
desc_length <- sapply(desc$product_description,n_words)
cat('Distribution of number of words in description\n')
print(quantile(desc_length,probs = seq(0,1,.1)))
hist(desc_length,main="Number of words in description", xlab="n_words",ylab="Frequency",col="skyblue1")


## Exploring attribute data
nids <- unique(attr$product_uid)
print(paste("Number of ids with attributes:",(length(nids)),sep=" "))
nattr <- sqldf('select product_uid,count(*) as n_attr from attr group by product_uid')
nattr <- nattr[!is.na(nattr$product_uid),]
cat('Distribution of number of attributes for products\n')
print(quantile(nattr$n_attr,probs = seq(0,1,.1)))
hist(nattr$n_attr,main="Number of attributes per product", xlab="N Attributes",ylab="Frequency",col="skyblue1")