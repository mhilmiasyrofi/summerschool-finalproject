install.packages("rjson")
install.packages("jsonlite")
install.packages("quanteda")
library("rjson")
library("jsonlite")
library("quanteda")


fileReview = "Cell_Phones_and_Accessories_5.json"
conn2 <- file(fileReview,open="r")
df1 = stream_in(conn2)
close(conn2)

View(head(df1))

fileName = "CellPhones.strict"

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

dtm.wordcloud(dtm = NULL, nterms = 100, freq.fun = NULL, terms = NULL,
              freqs = NULL, scale = c(6, 0.5), min.freq = 1, rot.per = 0.15,
              pal = brewer.pal(6, "YlGnBu"))
