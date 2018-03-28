search<-function(x){doc.list <- as.list(df$data)
object.size(doc.list)
N.docs <- length(doc.list)
names(doc.list) <- paste0("doc", c(1:N.docs))
query <- x
library(tm)
library(SnowballC)

my.docs <- VectorSource(c(doc.list, query))
my.docs$Names <- c(names(doc.list), "query")
my.corpus <- Corpus(my.docs)
my.corpus <- tm_map(my.corpus, removePunctuation)
my.corpus <- tm_map(my.corpus, stemDocument)
my.corpus <- tm_map(my.corpus, removeNumbers)
my.corpus <- tm_map(my.corpus, PlainTextDocument)
my.corpus <- tm_map(my.corpus, stripWhitespace)
term.doc.matrix.stm <- TermDocumentMatrix(my.corpus)
str(term.doc.matrix.stm)
term.doc.matrix <- as.matrix(term.doc.matrix.stm)
str(term.doc.matrix)
cat("Dense matrix representation costs", object.size(term.doc.matrix), "bytes.\n",
    "Simple triplet matrix representation costs", object.size(term.doc.matrix.stm),
    "bytes.")
View(term.doc.matrix)
get.tf.idf.weights <- function(tf.vec, df) {
  # Computes tfidf weights from a term frequency vector and a document
  # frequency scalar
  weight = rep(0, length(tf.vec))
  weight[tf.vec > 0] = (1 + log2(tf.vec[tf.vec > 0])) * log2(N.docs/df)
  weight
}
get.weights.per.term.vec <- function(tfidf.row) {
  term.df <- sum(tfidf.row[1:N.docs] > 0)
  tf.idf.vec <- get.tf.idf.weights(tfidf.row, term.df)
  return(tf.idf.vec)
}
tfidf.matrix <- t(apply(term.doc.matrix, c(1), FUN = get.weights.per.term.vec))
colnames(tfidf.matrix) <- colnames(term.doc.matrix)
angle <- seq(-pi, pi, by = pi/16)
plot(cos(angle) ~ angle, type = "b", xlab = "angle in radians", main = "Cosine similarity by angle")
tfidf.matrix <- scale(tfidf.matrix, center = FALSE, scale = sqrt(colSums(tfidf.matrix^2)))
query.vector <- tfidf.matrix[, (N.docs + 1)]
tfidf.matrix <- tfidf.matrix[, 1:N.docs]
doc.scores <- t(query.vector) %*% tfidf.matrix
results.df <- data.frame(doc = names(doc.list), score = t(doc.scores), text = unlist(doc.list))
results.df <- results.df[order(results.df$score, decreasing = TRUE), ]
tab<-head(results.df)
tab}
shinyServer(function(input,output,session){
  s <- readRDS("s.Rds")
  
  output$cc2<-renderTable({df<-search(input$x);df})

})