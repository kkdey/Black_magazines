
###################################   Ebony magazine   ##########################################

library(wordVectors)
dir <- list.files("../Ebony_ID/", pattern = ".bin")
w2vec <- list()
words <- c()
for(l in 1:16){
  w2vec[[l]] <- read.vectors(paste0("../Ebony_ID/", dir[l]))
  temp <- w2vec[[l]][which(nchar(rownames(w2vec[[l]])) > 2),]
  if(l==1){
    words <- rownames(temp)
  }else{
    words <- c(words, rownames(temp))
  }
}

num_words <- table(words)
words_filtered <- names(num_words[which(num_words > 10)])

word_pairs <- combn(words_filtered, 2)

final_w2vec <- list()
cor_final_w2vec <- list()
cor_vectors_append <- c()
for(l in 1:16){
  final_w2vec[[l]] <- w2vec[[l]][match(words_filtered, rownames(w2vec[[l]])),]
  cor_final_w2vec[[l]] <- as.matrix(cor(t(final_w2vec[[l]])))
  cor_vectors_append <- rbind(cor_vectors_append, cor_final_w2vec[[l]][lower.tri(cor_final_w2vec[[l]])])
  cat("We are at epoch", l, "\n")
}

save(cor_vectors_append, file = "../output/cor_vectors_append_ebony_ID.rda")

cor_vectors_append <- get(load("../output/cor_vectors_append_ebony_ID.rda"))
cor_vectors_append[is.na(cor_vectors_append)] <- 0

mean_cor_similarity <- colMeans(cor_vectors_append)

ordered_indices <- order(mean_cor_similarity, decreasing = TRUE)[1:500]
word_pairs[, ordered_indices]
mean_cor_similarity[ordered_indices]

df <- paste0(word_pairs[1, ordered_indices], ":", word_pairs[2, ordered_indices])
write.table(df, file = "../output/consistent_words/Ebony/highest_word_pairs_assoc.txt",
            row.names = FALSE, col.names = FALSE, quote = FALSE)

mean_cor_similarity <- colMeans(cor_vectors_append[1:8,])
ordered_indices <- order(mean_cor_similarity, decreasing = TRUE)[1:500]
word_pairs[, ordered_indices]
mean_cor_similarity[ordered_indices]
df2 <- paste0(word_pairs[1, ordered_indices], ":", word_pairs[2, ordered_indices])
write.table(df2, file = "../output/consistent_words/Ebony/highest_word_pairs_assoc_pre_1968.txt",
            row.names = FALSE, col.names = FALSE, quote = FALSE)


mean_cor_similarity <- colMeans(cor_vectors_append[9:16,])
ordered_indices <- order(mean_cor_similarity, decreasing = TRUE)[1:500]
word_pairs[, ordered_indices]
mean_cor_similarity[ordered_indices]
df3 <- paste0(word_pairs[1, ordered_indices], ":", word_pairs[2, ordered_indices])
write.table(df3, file = "../output/consistent_words/Ebony/highest_word_pairs_assoc_post_1968.txt",
            row.names = FALSE, col.names = FALSE, quote = FALSE)


######################   Black World and Negro Digest   ##################################

