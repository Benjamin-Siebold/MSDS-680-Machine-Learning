library(e1071)
library(caret)
library(tm)
library(SnowballC)
library(wordcloud)
library(gmodels)
setwd('/cloud/project/week_3')

texts <- read.delim('SMSSpamCollection', quote="", header= FALSE, stringsAsFactors=FALSE, col.names=c('type','text'))
texts$type <- factor(texts$type)
texts <- texts[1:1000, ]


text_corpus <- VCorpus(VectorSource(texts$text))
as.character(text_corpus[[1]])

text_corpus_clean <- tm_map(text_corpus, content_transformer(tolower))
text_corpus_clean <- tm_map(text_corpus_clean, removeNumbers)
text_corpus_clean <- tm_map(text_corpus_clean, removeWords, stopwords())
text_corpus_clean <- tm_map(text_corpus_clean, removePunctuation)


replacePunctuation <- function(x) {
  gsub("[[:punct:]]+", " ", x)
}

text_corpus_clean <- tm_map(text_corpus_clean, stemDocument)
text_corpus_clean <- tm_map(text_corpus_clean, stripWhitespace)

wordcloud(text_corpus_clean, min.freq = 50, random.order = FALSE)
spam <- subset(texts, type == 'spam')
ham <- subset(texts, type == 'ham')

wordcloud(spam$text, max.words = 40, scale = c(5,.5))
wordcloud(ham$text, max.words = 40, scale = c(3,.5))

text_matrix <- DocumentTermMatrix(text_corpus_clean)
dim(text_matrix)


###alternatively
text_matrix2 <- DocumentTermMatrix(text_corpus_clean, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePuncuation = TRUE,
  stemming = TRUE
))

split <- nrow(text_matrix)*.70
max_rows <- nrow(text_matrix)

text_matrix_train <- text_matrix[1:split, ]
text_matrix_test <- text_matrix[(split+1):max_rows, ]
text_train_labels <- texts[1:split, ]$type
text_test_labels <- texts[(split+1):max_rows, ]$type

prop.table(table(text_train_labels))
prop.table(table(text_test_labels))

text_freq_words <- findFreqTerms(text_matrix_train, 5)
text_matrix_freq_train <- text_matrix_train[, text_freq_words]
text_matrix_freq_test <- text_matrix_test[, text_freq_words]

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

text_train <- apply(text_matrix_freq_train, MARGIN=2, convert_counts)
text_test <- apply(text_matrix_freq_test, MARGIN=2, convert_counts)

text_classifier <- naiveBayes(text_train, text_train_labels)
text_pred <- predict(text_classifier, text_test)

CrossTable(text_pred, text_test_labels, prop.chisq=FALSE, prop.t=FALSE, dnn=c('predicted','actual'))
confusionMatrix(text_pred, text_test_labels)

text_classifier2 <- naiveBayes(text_train, text_train_labels, laplace = 1)
text_pred2 <- predict(text_classifier2, text_test)

confusionMatrix(text_pred2, text_test_labels)
