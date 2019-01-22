library (tm)
library (dplyr)
library (tidyr)
library (ngram)
library (stringr)
library (tictoc)

# *** Set constants. This code snippet is not required to be displayed on HTML
scriptDir <- "F:/01. Data Science/10. Data Science Capstone/05. Capstone" # Working directory location of R script and dataset
outputDirName <- "./sample"
setwd (scriptDir)

rdsCombinedWithStopWordsUni <- "./sample/rdsCombinedWithStopWordsUni.rds"
rdsCombinedWithStopWordsBi <- "./sample/rdsCombinedWithStopWordsBi.rds"
rdsCombinedWithStopWordsTri <- "./sample/rdsCombinedWithStopWordsTri.rds"
rdsCombinedWithStopWordsQuad <- "./sample/rdsCombinedWithStopWordsQuad.rds"

rdsCombinedWithoutStopWordsUni <- "./sample/rdsCobinedWithoutStopWordsUni.rds"
rdsCombinedWithoutStopWordsBi <- "./sample/rdsCobinedWithoutStopWordsBi.rds"
rdsCombinedWithoutStopWordsTri <- "./sample/rdsCobinedWithoutStopWordsTri.rds"
rdsCombinedWithoutStopWordsQuad <- "./sample/rdsCobinedWithoutStopWordsQuad.rds"

nGramCombinedWithStopWordsUni <- readRDS (rdsCombinedWithStopWordsUni)
nGramCombinedWithStopWordsBi <- readRDS (rdsCombinedWithStopWordsBi)
nGramCombinedWithStopWordsTri <- readRDS (rdsCombinedWithStopWordsTri)
nGramCombinedWithStopWordsQuad <- readRDS (rdsCombinedWithStopWordsQuad)

nGramCombinedWithStopWordsBi <- nGramCombinedWithStopWordsBi %>% separate (Word, c ("word1", "word2"), sep = " ")
nGramCombinedWithStopWordsTri <- nGramCombinedWithStopWordsTri %>% separate (Word, c ("word1", "word2", "word3"), sep = " ")
nGramCombinedWithStopWordsQuad <- nGramCombinedWithStopWordsQuad %>% separate (Word, c ("word1", "word2", "word3", "word4"), sep = " ")

nGramCombinedWithoutStopWordsUni <- readRDS (rdsCombinedWithoutStopWordsUni)
nGramCombinedWithoutStopWordsBi <- readRDS (rdsCombinedWithoutStopWordsBi)
nGramCombinedWithoutStopWordsTri <- readRDS (rdsCombinedWithoutStopWordsTri)
nGramCombinedWithoutStopWordsQuad <- readRDS (rdsCombinedWithoutStopWordsQuad)

nGramCombinedWithoutStopWordsBi <- nGramCombinedWithoutStopWordsBi %>% separate (Word, c ("word1", "word2"), sep = " ")
nGramCombinedWithoutStopWordsTri <- nGramCombinedWithoutStopWordsTri %>% separate (Word, c ("word1", "word2", "word3"), sep = " ")
nGramCombinedWithoutStopWordsQuad <- nGramCombinedWithoutStopWordsQuad %>% separate (Word, c ("word1", "word2", "word3", "word4"), sep = " ")

fnPredictNextWord <- function (inputString) {
  
  # Clean the input string
  # Prepare Corpus
  corpusText <- inputString
  corpusFeeds <- VCorpus (VectorSource(corpusText))
  # Clean string
  corpusFeeds <- tm_map (corpusFeeds, removePunctuation) # Remove punctuations
  corpusFeeds <- tm_map (corpusFeeds, content_transformer(tolower)) # Convert text to lower case
  corpusFeeds <- tm_map (corpusFeeds, content_transformer(remove_internet_chars)) # Remove internet specific characters
  corpusFeeds <- tm_map (corpusFeeds, content_transformer(remove_symbols)) # Remove symbols such as ?????, etc.
  corpusFeeds <- tm_map (corpusFeeds, stripWhitespace) # Eliminate extra white spaces  
  
  inputString <- as.character(corpusFeeds [[1]])
  inputStringLength <- wordcount (inputString)

  if (inputStringLength > 3) { # Use Quad Gram
    predictedWords <- fnPredicFromQuadGrams (word (inputString, wordcount(inputString)-2), word (inputString, wordcount(inputString)-1), word (inputString, wordcount(inputString)))
  } 
  else if (inputStringLength == 3) { # Use Quad Gram
    predictedWords <- fnPredicFromQuadGrams (word(inputString, 1), word(inputString, 2), word(inputString, 3))
  }
  else if (inputStringLength == 2) { # Use Tri Gram
    predictedWords <- fnPredicFromTriGrams (word(inputString, 1), word(inputString, 2))
  }
  else if (inputStringLength == 1) { # Use Bi Gram
    predictedWords <- fnPredicFromBiGrams (word(inputString, 1))    
  }
  else if (inputStringLength == 0) { # Error
    print ("Error: No input string found for prediction")
  }
  
  if (inputStringLength != 0) {
    print (predictedWords)
  }
  
  return (predictedWords)
}


fnPredicFromQuadGrams <- function (inputWord1, inputWord2, inputWord3) {
  
  predictedWordFromStopWords <- filter(nGramCombinedWithStopWordsQuad, (word1 == inputWord1 & word2 == inputWord2 & word3 == inputWord3))$word4
  predictedWordFromWithoutStopWords <- filter(nGramCombinedWithoutStopWordsQuad, (word1 == inputWord1 & word2 == inputWord2 & word3 == inputWord3))$word4
  predictions <- list (predictedWordFromStopWords, predictedWordFromWithoutStopWords)
  
  return (predictions)
}

fnPredicFromTriGrams <- function (inputWord1, inputWord2) {
  
  predictedWordFromStopWords <- filter(nGramCombinedWithStopWordsTri, (word1 == inputWord1 & word2 == inputWord2))$word3
  predictedWordFromWithoutStopWords <- filter(nGramCombinedWithoutStopWordsTri, (word1 == inputWord1 & word2 == inputWord2))$word3
  predictions <- list (predictedWordFromStopWords, predictedWordFromWithoutStopWords)
  
  return (predictions)
}

fnPredicFromBiGrams <- function (inputWord1) {
  
  predictedWordFromStopWords <- filter(nGramCombinedWithStopWordsBi, (word1 == inputWord1))$word2
  predictedWordFromWithoutStopWords <- filter(nGramCombinedWithoutStopWordsBi, (word1 == inputWord1))$word2
  predictions <- list (predictedWordFromStopWords, predictedWordFromWithoutStopWords)
  
  return (predictions)
}


remove_internet_chars <- function(x){
  # replace emails and such but space
  x <- gsub("[^ ]{1,}@[^ ]{1,}"," ",x)
  x <- gsub(" @[^ ]{1,}"," ",x)
  # hashtags
  x <- gsub("#[^ ]{1,}"," ",x) 
  # websites and file systems
  x <- gsub("[^ ]{1,}://[^ ]{1,}"," ",x) 
  x
}

remove_symbols <- function(x){
  # Edit out most non-alphabetical character
  # text must be lower case first
  x <- gsub("[`??????]","'",x)
  x <- gsub("[^a-z']"," ",x)
  x <- gsub("'{2,}"," '",x)
  x <- gsub("' "," ",x)
  x <- gsub(" '"," ",x)
  x <- gsub("^'","",x)
  x <- gsub("'$","",x)
  x
}

tic ("Started")
prediction <- fnPredictNextWord ("btw thanks for") 
toc ()
