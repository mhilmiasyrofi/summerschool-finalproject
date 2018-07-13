# ======================
## Required Packages
# ======================

## Library for NLP to extract key text from sentence
# install.packages("rJava")
# install.packages("openNLP")
# install.packages("NLP")

library(rJava)
require("openNLP")
require("NLP")


## Library to get some insight from Amazon Data
# install.packages("rjson")
# install.packages("jsonlite")
# install.packages("quanteda")

library("rjson")
library("jsonlite")
library("quanteda")
library("wordcloud")

# =========================================
## Part 1 - Extract Key Text from Sentence
# =========================================

## Function to get the key text from sentence
extractChunks <- function(x) {
  
  x <- as.String(x)
  wordAnnotation <- annotate(x, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
  POSAnnotation <- annotate(x, Maxent_POS_Tag_Annotator(), wordAnnotation)
  POSwords <- subset(POSAnnotation, type == "word")
  tags <- sapply(POSwords$features, '[[', "POS")
  tokenizedAndTagged <- data.frame(Tokens = x[POSwords], Tags = tags)
  
  tokenizedAndTagged$Tags_mod = grepl("NN|JJ", tokenizedAndTagged$Tags)
  chunk = vector()
  
  chunk[1] = as.numeric(tokenizedAndTagged$Tags_mod[1])
  
  for (i in 2:nrow(tokenizedAndTagged)) {
    
    if(!tokenizedAndTagged$Tags_mod[i]) {
      chunk[i] = 0
    } else if (tokenizedAndTagged$Tags_mod[i] == tokenizedAndTagged$Tags_mod[i-1]) {
      chunk[i] = chunk[i-1]
    } else {
      chunk[i] = max(chunk) + 1
    }
    
  }
  
  text_chunk <- split(as.character(tokenizedAndTagged$Tokens), chunk)
  tag_pattern <- split(as.character(tokenizedAndTagged$Tags), chunk)
  names(text_chunk) <- sapply(tag_pattern, function(x) paste(x, collapse = "-"))
  
  # Extract chunks matching pattern
  res = text_chunk[grepl("JJ-NN|NN.-NN", names(text_chunk))]
  res = sapply(res, function(x) paste(x, collapse =  " "))
  return(res)
  
  gc()
  
}

main = extractChunks("Complimentary gym access for two for the length of stay $12 value per person per day")



# ===============================================
## Part 2 - Finding The Insight From Amazon Data
# ===============================================

fileReview = "data/Cell_Phones_and_Accessories_5.json"
conn2 <- file(fileReview,open="r")
df1 = stream_in(conn2)
close(conn2)

# View(head(df1))

fileName = "data/CellPhones.strict"

conn <- file(fileName,open="r")
linn <-readLines(conn)
first = TRUE
for (i in 1:length(linn)){
  if (i < 70000) {
    js = fromJSON(linn[i])
    if (!is.null(js$brand)) {
      if (js$brand == "Samsung" | js$brand == "Apple" | js$brand == "Nokia" ) {
        if (first) {
          # print(js)
          df2<-data.frame(js$asin, js$brand)
          names(df2) = c("asin", "Brand")
          first = FALSE
        } else {
          de<-data.frame(js$asin, js$brand)
          names(de) = c("asin", "Brand")
          newdf = rbind(df2, de)
          df2 = newdf
        } 
      }
    }
  }
}
close(conn)

# View(head(df1))
# View(head(df2))

df = merge(df1, df2)
# View(head(df))

df = data.frame(ID = df$asin, Review = df$reviewText, Brand = df$Brand, Rating = df$overall)

review = tokens(as.character(df$Review), remove_number = T, remove_symbols = TRUE)

dtm <- dfm(review, tolower = TRUE, stem = TRUE,   # create dtm with preprocessing
           remove_punct = TRUE, remove = stopwords(source = "smart")) 

topfeatures(dtm, 10)

doc_freq <- docfreq(dtm)         # document frequency per term (column) 
dtm <- dtm[, doc_freq >= 30]      # select terms with doc_freq >= 2 
dtm <- dfm_weight(dtm, "tfidf")  # weight the features using tf-idf 
## Warning: scheme = "tfidf" is deprecated; use dfm_tfidf(x) instead
# head(dtm)

col <- sapply(seq(0.5, 2, 0.5), function(x) adjustcolor("#1F78B4", x))

# first we visualize topic model for the reivew to get the main topics
# unforunately it is not good enough
# we decide to make own category
textplot_wordcloud(dtm, color = col)


# ===================================================
# Make category

category = c("service", "design", "function", "package", "quality")

cats = c()

for (x in df$Review) {
  cat = c()
  for (c in category) {
    if (grepl(c, tolower(x))) {
      cat[length(cat)+1] = c
      break
    }
  }
  if (length(cat) == 0) {
    cat = c("None")
  }
  cats[length(cats)+1] = cat
}

df$Category = cats

dfSamsung = df[df$Brand == "Samsung",]
dfApple = df[df$Brand == "Apple",]
dfNokia = df[df$Brand == "Nokia",]

# Rating per brands
meanRatingSamsung = mean(dfSamsung$Rating)
meanRatingApple = mean(dfApple$Rating)
meanRatingNokia = mean(dfNokia$Rating)

# Calculate average value of rating from each category for each brands
ratesSamsung = c()
for (c in category) {
  ratesSamsung[length(ratesSamsung)+1] = round(mean(dfSamsung$Rating[dfSamsung$Category == c], na.rm = T),2)
}

ratesApple = c()
for (c in category) {
  ratesApple[length(ratesApple)+1] = round(mean(dfApple$Rating[dfApple$Category == c], na.rm = T), 2)
}

ratesNokia = c()
for (c in category) {
  ratesNokia[length(ratesNokia)+1] = round(mean(dfNokia$Rating[dfNokia$Category == c], na.rm = T), 2)
}

print(category)
print(ratesSamsung)
print(ratesApple)
print(ratesNokia)

summary = data.frame(category, ratesSamsung, ratesApple, ratesNokia)

View(summary)
