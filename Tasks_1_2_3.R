library (readr)
library (tm)
library(wordcloud)
library(RWeka)
library (stringi)
library (ggplot2)
library (gridExtra)

# *** Set constants
# Working directory location of R script and dataset - en_US.twitter.txt
scriptDir <- "F:/01. Data Science/10. Data Science Capstone/05. Capstone"
# File used in creating sample dataset
inputFileNameTwitter <- "./final/en_US/en_US.twitter.txt"
inputFileNameNews <- "./final/en_US/en_US.news.txt"
inputFileNameBlogs <- "./final/en_US/en_US.blogs.txt"
# Sample file which will contain randomly read lines
sampleFileNameTwitter <- "./sample/sampleDatasetTwitter.txt"
sampleFileNameNews <- "./sample/sampleDatasetNews.txt"
sampleFileNameBlogs <- "./sample/sampleDatasetBlogs.txt"
outputDirName <- "./sample"
noOfRandomLines <- 100
setwd (scriptDir)
badWordsFileURL <- "http://www.bannedwordlist.com/lists/swearWords.txt"
badWordsFileName <- "badwords.txt"


fnDownloadDataSet <- function () {
  
  setwd(scriptDir)
  if (!file.exists("Coursera-SwiftKey.zip")) {
    download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", "Coursera-SwiftKey.zip")
    unzip("Coursera-SwiftKey.zip")
  }
}

fnDownloadDataSet ()

# Function to get size, number of lines and number of words in dataset files
fnGetOriginalDatasetInfo <- function () {
  conTwitter <- file (inputFileNameTwitter)
  conNews <- file (inputFileNameNews)
  conBlogs <- file (inputFileNameBlogs)
  
  linesTwitter <- readLines (conTwitter)
  linesNews <- readLines(conNews)
  linesBlogs <- readLines (conBlogs)
  
  datasetTwitterSummary <- fnGetFileInfo (linesTwitter)
  datasetNewsSummary <- fnGetFileInfo (linesNews)
  datasetBlogsSummary <- fnGetFileInfo (linesBlogs)
  
  tableRows <- c ("Twitter", "News", "Blogs")
  datasetInfo <- rbind (unlist(datasetTwitterSummary), unlist(datasetNewsSummary), unlist(datasetBlogsSummary))
  datasetTable <- data.frame (cbind(tableRows, datasetInfo))
  names (datasetTable) <- c ("File", "Size", "Number of Lines", "Number of Words")
  datasetTable
  
  close (conTwitter)
  close (conNews)
  close (conBlogs)
}

fnGetFileInfo <- function (datasetText) {
  fileSize <- format (object.size(datasetText), units = "Mb")
  fileLines <- length(datasetText)
  fileWords <- sum(stri_count_words(datasetText))
  return (list(fileSize, fileLines, fileWords))
}

# Function to read dataset txt file, read random 100 lines from that file. These lines will be written to a sampleDataset.txt file for tokenization, etc.
fnCreateSampleDataset <- function (inputFileName) {
  
  # Generate sample dataset from Twitter file
  conTwitter <- file (inputFileNameTwitter)
  linesTwitter <- readLines (conTwitter)
  open (conTwitter, "r")
  noOfRowsInFile <- length (readLines(conTwitter))
  randomLineNumbers <- sample (1:noOfRowsInFile, noOfRandomLines, replace = F)
  close (conTwitter)
  # Read random 100 lines from the file and store it in a sample file
  for (i in 1:100) {
    tempLine <- read_lines (inputFileNameTwitter, skip = randomLineNumbers[i], skip_empty_rows = FALSE, n_max = 1)
    cat (i, " - ", randomLineNumbers[i], " - ", tempLine, "\n")
    write_lines (tempLine, sampleFileNameTwitter, sep = "\r\n", na = "NA", append = TRUE)
  } 
  
  # Generate sample dataset from News file
  conNews <- file (inputFileNameNews)
  linesNews <- readLines(conNews)
  open (conNews, "r")
  noOfRowsInFile <- length (readLines(conNews))
  randomLineNumbers <- sample (1:noOfRowsInFile, noOfRandomLines, replace = F)
  close (conNews)
  # Read random 100 lines from the file and store it in a sample file
  for (i in 1:100) {
    tempLine <- read_lines (inputFileNameNews, skip = randomLineNumbers[i], skip_empty_rows = FALSE, n_max = 1)
    write_lines (tempLine, sampleFileNameNews, sep = "\r\n", na = "NA", append = TRUE)
  }
  
  # Generate sample dataset from Blogs file
  conBlogs <- file (inputFileNameBlogs)
  linesBlogs <- readLines (conBlogs)  
  noOfRowsInFile <- length (readLines(conBlogs))
  randomLineNumbers <- sample (1:noOfRowsInFile, noOfRandomLines, replace = F)
  close (conBlogs)
  for (i in 1:100) {
    tempLine <- read_lines (inputFileNameBlogs, skip = randomLineNumbers[i], skip_empty_rows = FALSE, n_max = 1)
    write_lines (tempLine, sampleFileNameBlogs, sep = "\r\n", na = "NA", append = TRUE)
  }
}

# Week 1
fnGetOriginalDatasetInfo ()
fnCreateSampleDataset ()
##################################################################################################################


# Week 2 - Task 1 - Exploratory Data Analysis
fnExploratoryAnalysis <- function (datasetFileName) {
  # Prepare Corpus
  corpusText <- readLines (datasetFileName)
  corpusFeeds <- VCorpus (VectorSource(corpusText))

  # dtm_corpusFeeds <- DocumentTermMatrix (corpusFeeds)   
  # cat ("1 Number of terms ", dtm_corpusFeeds$nrow)
  
  # Clean dataset 
  corpusFeeds <- tm_map (corpusFeeds, removePunctuation) # Remove punctuations
  corpusFeeds <- tm_map (corpusFeeds, content_transformer(tolower)) # Convert text to lower case
  corpusFeeds <- tm_map (corpusFeeds, content_transformer(remove_internet_chars)) # Remove internet specific characters
  corpusFeeds <- tm_map (corpusFeeds, removeWords, stopwords("english")) # Remove symbols such as ?????, etc.
  corpusFeeds <- tm_map (corpusFeeds, content_transformer(remove_symbols)) # Remove common words in English
  corpusFeeds <- tm_map (corpusFeeds, stripWhitespace) # Eliminate extra white spaces
  
  # Profinity filtering - Can use addspace custom made function as required
  if ( !file.exists(badWordsFileName)) {
    # fileUrl2 <- "http://www.bannedwordlist.com/lists/swearWords.txt"
    download.file(badWordsFileURL, destfile = badWordsFileName)
  }
  badwords <- readLines(badWordsFileName)
  profanity <- VectorSource(badwords)
  corpusFeeds <- tm_map(corpusFeeds, removeWords, profanity)
  
  corpusFeeds <- tm_map(corpusFeeds, PlainTextDocument) # Convert data to plain text
  return (corpusFeeds)
  
  # Coerce corpusFeeds into a Document Term Matrix
  #dtm_corpusFeeds <- DocumentTermMatrix (corpusFeeds)   
  #return (dtm_corpusFeeds)
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

corpusTwitter <- fnExploratoryAnalysis (sampleFileNameTwitter)
corpusNews <- fnExploratoryAnalysis (sampleFileNameNews)
corpusBlogs <- fnExploratoryAnalysis (sampleFileNameBlogs)
corpusCombined <- c (corpusTwitter, corpusNews, corpusBlogs)

fnGenerateNGramPlots <- function (corpusForNGrams) {
  
  dataFrameForNGrams <- data.frame (text = sapply (corpusForNGrams, as.character), stringsAsFactors = FALSE)
  uniGramToken <- NGramTokenizer (dataFrameForNGrams, Weka_control (min=1, max=1))
  biGramToken <- NGramTokenizer (dataFrameForNGrams, Weka_control(min=2, max=2))
  triGramToken <- NGramTokenizer (dataFrameForNGrams, Weka_control(min=3, max=3))
  
  uniGrams <- data.frame(table(uniGramToken))
  biGrams <- data.frame(table(biGramToken))
  triGrams <- data.frame(table(triGramToken))
  
  uniGrams <- uniGrams[order(uniGrams$Freq,decreasing=TRUE),]
  colnames(uniGrams) <- c("Word", "Frequency")
  biGrams <- biGrams[order(biGrams$Freq,decreasing=TRUE),]
  colnames(biGrams) <- c("Word", "Frequency")
  triGrams <- triGrams[order(triGrams$Freq,decreasing=TRUE),]
  colnames(triGrams) <- c("Word", "Frequency")
  
  uniGrams_s <- uniGrams[1:50,]
  biGrams_s <- biGrams[1:50,]
  triGrams_s <- triGrams[1:50,]
  
  p1 = ggplot (uniGrams_s, aes (x = reorder (Word, Frequency), y = Frequency)) + geom_bar(stat="identity", fill="red") + 
    geom_text (aes(y = Frequency + 10, label = Frequency), vjust = 1) + coord_flip() + labs (x = "Word", y = "Frequency", title = "biGrams Frequency")

  
    p2 = ggplot (biGrams_s, aes (x = reorder (Word, Frequency), y = Frequency)) + geom_bar(stat="identity", fill="green") + 
    geom_text (aes(y = Frequency + 20, label = Frequency), vjust = 1) + coord_flip() + labs(x = "Word", y = "Frequency", title = "biGrams Frequency")
  p3 = ggplot (triGrams_s, aes (x = reorder (Word, Frequency), y=Frequency)) + geom_bar(stat="identity", fill="blue") + 
    geom_text (aes(y=Frequency + 30, label = Frequency), vjust=1) + coord_flip() + labs (x = "Word", y = "Frequency", title = "triGrams Frequency")
  
  grid.arrange (p1, p2, p3)
  
  #par(mfrow = c(2, 1))
  #wordcloud(words = uniGrams$Word, freq = uniGrams$Frequency,
  #          max.words=200, random.order=FALSE, colors = brewer.pal(4, "Set1"), main = "uniGrams")
  #wordcloud(words = biGrams$Word, freq = biGrams$Frequency,
  #          max.words=200, random.order=FALSE, colors = brewer.pal(4, "Set1"), main = "biGrams")
  #wordcloud(words = triGrams$Word, freq = triGrams$Frequency,
  #          max.words=200, random.order=FALSE, colors = brewer.pal(4, "Set1"), main = "triGrams")
  
  return
}


fnGenerateNGramPlots (corpusCombined)



