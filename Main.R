install.packages("rjson")
install.packages("jsonlite")
install.packages("quanteda")
install.packages("wordcloud")
library("rjson")
library("jsonlite")
library("quanteda")
library("wordcloud")


fileReview = "data/Cell_Phones_and_Accessories_5.json"
conn2 <- file(fileReview,open="r")
df1 = stream_in(conn2)
close(conn2)

View(head(df1))

fileName = "data/CellPhones.strict"

conn <- file(fileName,open="r")
linn <-readLines(conn)
first = TRUE
for (i in 1:length(linn)){
  if (i < 50000) {
    js = fromJSON(linn[i])
    if (!is.null(js$brand)) {
      if (js$brand == "Samsung" || js$brand == "Apple" || js$brand == "Nokia" ) {
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


View(head(df1))
View(head(df2))

df = merge(df1, df2)
View(head(df))

df = data.frame(ID = df$asin, Review = df$reviewText, Brand = df$Brand, Rating = df$overall)
View((df))
View(head(df))

test = "akk"
val = "aku dan kamu"

grepl(test, val)

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

print(length(cats))

# View(head(cats))

df$Category = cats

# View(head(df))
View(df)

dtm <- dfm(as.character(df$Review), tolower = TRUE, stem = TRUE,   # create dtm with preprocessing
           remove_punct = TRUE, remove = stopwords(source = "smart")) 

topfeatures(dtm, 10)

?dfm

doc_freq <- docfreq(dtm)         # document frequency per term (column) 
dtm <- dtm[, doc_freq >= 40]      # select terms with doc_freq >= 2 
dtm <- dfm_weight(dtm, "tfidf")  # weight the features using tf-idf 
## Warning: scheme = "tfidf" is deprecated; use dfm_tfidf(x) instead
# head(dtm)

col <- sapply(seq(0.5, 1, 0.5), function(x) adjustcolor("#1F78B4", x))

textplot_wordcloud(dtm, color = col)

?wordcloud


