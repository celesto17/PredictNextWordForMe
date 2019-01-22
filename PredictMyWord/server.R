# Course 10 - Data Science Capstone
# Author: Sanjay Lonkar
# Date: 20-Jan-2018

library(shiny)
library (tm)
library (dplyr)
library (tidyr)
library (ngram)
library (stringr)
library (tictoc)

# *** Set constants. This code snippet is not required to be displayed on HTML
tic ("Preparing N Gram Corpora")
#scriptDir <- "F:/01. Data Science/10. Data Science Capstone/05. Capstone" # Working directory location of R script and dataset
outputDirName <- "./sample"
#setwd (scriptDir)

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

toc ()

# Define server logic required to predict word
shinyServer(function(input, output) {
   
  # TAB 1 - Predict Word
  output$tabPrediction <- renderUI ({ 
    
    input$btnPredict
    isolate({
      tic ("Time taken for word prediction in seconds: ")
      inputString <- input$txtInputString
      commonWordsToBeConsidered  <- input$rdoStopWords # 0 - Consider stop words, 1 - Do not consider stop words
      noOfWordsToPredict <- input$numWordsToPredict
      
      predictedWordList <- fnPredictNextWord (inputString, commonWordsToBeConsidered)
      
      if (commonWordsToBeConsidered == 0) {
        
        # With stop words
        predictedWordsFromCommonWords <- "Using Common Words: "
        predictedWordsFromCommonWordsLimit <- ""
        
        if (length(predictedWordList [[1]] != 0)) {
          for (i in 1 : noOfWordsToPredict) {
            if  (length (predictedWordList[[1]]) >= i) {
              predictedWordsFromCommonWords <- paste (predictedWordsFromCommonWords, predictedWordList [[1]][i], sep = ", ")
            }
            else {
              predictedWordsFromCommonWordsLimit <- paste ("Words that could be predicted are ", i-1, " instead of required ", noOfWordsToPredict, " words.")
              break
            }
          }
        }
        else {
          predictedWordsFromCommonWordsLimit <- "Sorry, no prediction found from common words."
        }
        
        # Without stop words
        predictedWordsWithoutCommonWords <- "Without Using Common Words: "
        predictedWordsWithoutCommonWordsLimit <- ""
        
        if (length(predictedWordList [[2]] != 0)) {
          for (i in 1 : noOfWordsToPredict) {
            if ((length (predictedWordList [[2]])) >= i) {
              predictedWordsWithoutCommonWords <- paste (predictedWordsWithoutCommonWords, predictedWordList [[2]][i], sep = ", ")
            }
            else {
              predictedWordsWithoutCommonWordsLimit <- paste ("Words that could be predicted are ", i-1, " instead of required ", noOfWordsToPredict, " words.")
              break
            }
          }
        }
        else {
          predictedWordsWithoutCommonWordsLimit <- "Sorry, no prediction found from without using common words."
        }

        predictedWordsFromCommonWords <- str_replace (predictedWordsFromCommonWords, ": ,", ":")
        predictedWordsWithoutCommonWords <- str_replace (predictedWordsWithoutCommonWords, ": ,", ":")
        
        performanceMeasure <- toc ()
        performanceTime <- performanceMeasure$toc - performanceMeasure$tic
        performanceMeasure <- paste ("Time taken for word prediction algoritm in seconds: ", performanceTime)
        closureMsg <- paste ("Prediction compelte for '", inputString, "'.")
        
        HTML (paste (predictedWordsFromCommonWords, predictedWordsFromCommonWordsLimit, "<br/>", predictedWordsWithoutCommonWords, predictedWordsWithoutCommonWordsLimit, "<br/>", performanceMeasure, closureMsg, sep = "<br/>"))
      }
      else { # Stop words not to be considered
        
        # Without stop words
        predictedWordsWithoutCommonWords <- "Without Using Common Words: "
        predictedWordsWithoutCommonWordsLimit <- ""
        
        if (length(predictedWordList [[1]] != 0)) {
          for (i in 1 : noOfWordsToPredict) {
            if ((length (predictedWordList [[1]])) >= i) {
              predictedWordsWithoutCommonWords <- paste (predictedWordsWithoutCommonWords, predictedWordList [[1]][i], sep = ", ")
            }
            else {
              predictedWordsWithoutCommonWordsLimit <- paste ("Words that could be predicted are ", i-1, " instead of required ", noOfWordsToPredict, " words.")
              break
            }
          }
        }
        else {
          predictedWordsWithoutCommonWordsLimit <- "Sorry, no prediction found from without using common words."
        }
        
        predictedWordsWithoutCommonWords <- str_replace (predictedWordsWithoutCommonWords, ": ,", ":")
        
        performanceMeasure <- toc ()
        performanceTime <- performanceMeasure$toc - performanceMeasure$tic
        performanceMeasure <- paste ("Time taken for word prediction algoritm in seconds: ", performanceTime)
        closureMsg <- paste ("Prediction compelte for '", inputString, "'.")
        
        HTML (paste (predictedWordsWithoutCommonWords, "<br/>", predictedWordsWithoutCommonWordsLimit, performanceMeasure, closureMsg, sep = "<br/>"))
      }
    })
  })
  
})

fnPredictNextWord <- function (inputString, commonWordsToBeConsidered) {
  
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
    predictedWords <- fnPredicFromQuadGrams (word (inputString, inputStringLength-2), word (inputString, inputStringLength-1), word (inputString, inputStringLength), commonWordsToBeConsidered)
  } 
  else if (inputStringLength == 3) { # Use Quad Gram
    predictedWords <- fnPredicFromQuadGrams (word(inputString, 1), word(inputString, 2), word(inputString, 3), commonWordsToBeConsidered)
  }
  else if (inputStringLength == 2) { # Use Tri Gram
    predictedWords <- fnPredicFromTriGrams (word(inputString, 1), word(inputString, 2), commonWordsToBeConsidered)
  }
  else if (inputStringLength == 1) { # Use Bi Gram
    predictedWords <- fnPredicFromBiGrams (word(inputString, 1), commonWordsToBeConsidered)    
  }
  else if (inputStringLength == 0) { # Error
    predictedWords <- "Error: No input string found for prediction"
  }

  return (predictedWords)
}


fnPredicFromQuadGrams <- function (inputWord1, inputWord2, inputWord3, commonWordsToBeConsidered) {
  
  if (commonWordsToBeConsidered == 0) {
    predictedWordFromStopWords <- filter(nGramCombinedWithStopWordsQuad, (word1 == inputWord1 & word2 == inputWord2 & word3 == inputWord3))$word4
    predictedWordFromWithoutStopWords <- filter(nGramCombinedWithoutStopWordsQuad, (word1 == inputWord1 & word2 == inputWord2 & word3 == inputWord3))$word4
    predictions <- list (predictedWordFromStopWords, predictedWordFromWithoutStopWords)
  }
  else if (commonWordsToBeConsidered == 1) {
    predictedWordFromWithoutStopWords <- filter(nGramCombinedWithoutStopWordsQuad, (word1 == inputWord1 & word2 == inputWord2 & word3 == inputWord3))$word4
    predictions <- list (predictedWordFromWithoutStopWords)
  }
  
  return (predictions)
}

fnPredicFromTriGrams <- function (inputWord1, inputWord2, commonWordsToBeConsidered) {

  if (commonWordsToBeConsidered == 0) {
    predictedWordFromStopWords <- filter(nGramCombinedWithStopWordsTri, (word1 == inputWord1 & word2 == inputWord2))$word3
    predictedWordFromWithoutStopWords <- filter(nGramCombinedWithoutStopWordsTri, (word1 == inputWord1 & word2 == inputWord2))$word3
    predictions <- list (predictedWordFromStopWords, predictedWordFromWithoutStopWords)
  }
  else if (commonWordsToBeConsidered == 1) {
    predictedWordFromWithoutStopWords <- filter(nGramCombinedWithoutStopWordsTri, (word1 == inputWord1 & word2 == inputWord2))$word3
    predictions <- list (predictedWordFromWithoutStopWords)  
  }
  
  return (predictions)
}

fnPredicFromBiGrams <- function (inputWord1, commonWordsToBeConsidered) {
  
  if (commonWordsToBeConsidered == 0) {
    predictedWordFromStopWords <- filter(nGramCombinedWithStopWordsBi, (word1 == inputWord1))$word2
    predictedWordFromWithoutStopWords <- filter(nGramCombinedWithoutStopWordsBi, (word1 == inputWord1))$word2
    predictions <- list (predictedWordFromStopWords, predictedWordFromWithoutStopWords)
  }
  else if (commonWordsToBeConsidered == 1) {
    predictedWordFromWithoutStopWords <- filter(nGramCombinedWithoutStopWordsBi, (word1 == inputWord1))$word2
    predictions <- list (predictedWordFromWithoutStopWords)  
  }
  
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