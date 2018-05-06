library(tm)
library(wordcloud)
library(memoise)

# The list of valid books

books <<- list("2018/4/13"= "text_0413",
               "2018/4/14"= "text_0414",
               "2018/4/15"= "text_0415",
               "2018/4/16"= "text_0416",
               "2018/4/17"= "text_0417",
               "2018/4/18"= "text_0418",
               "2018/4/19"= "text_0419",
               "2018/4/20"= "text_0420",
               "2018/4/21"= "text_0421",
               "2018/4/22"= "text_0422",
               "2018/4/23"= "text_0423",
               "2018/4/24"= "text_0424",
               "2018/4/25"= "text_0425",
               "2018/4/26"= "text_0426",
               "2018/4/27"= "text_0427",
               "2018/4/28"= "text_0428",
               "2018/4/29"= "text_0429",
               "2018/4/30"= "text_0430",
               "2018/5/01"= "text_0501",
               "2018/5/02"= "text_0502")

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(book) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  if (!(book %in% books))
    stop("Unknown book")
  
  text <- readLines(sprintf("./%s.txt", book),
                    encoding="UTF-8")
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})


