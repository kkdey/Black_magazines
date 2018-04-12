

####################### all word2vec similarities  ###############################

############  Ebony  #################

dir <- c(list.files("../Negro_Digest/", pattern = "ND"), list.files("../Black_World/", pattern = "BL"))
library(wordVectors)
w2vec <- list()
words <- c()
for(l in 1:17){
  if(l <=10){
    w2vec[[l]] <- read.vectors(paste0("../Negro_Digest/", dir[l], "/pooled_word2vec.bin"))
  }else{
    w2vec[[l]] <- read.vectors(paste0("../Black_World/", dir[l], "/pooled_word2vec.bin"))
  }
  temp <- w2vec[[l]][which(nchar(rownames(w2vec[[l]])) > 2),]
  if(l==1){
    words <- rownames(temp)
  }else{
    words <- c(words, rownames(temp))
  }
}

num_words <- table(words)
words_filtered <- names(num_words[which(num_words > 13)])

word_pairs <- combn(words_filtered, 2)

final_w2vec <- list()
cor_final_w2vec <- list()
cor_vectors_append <- c()
for(l in 1:17){
  final_w2vec[[l]] <- w2vec[[l]][match(words_filtered, rownames(w2vec[[l]])),]
  cor_final_w2vec[[l]] <- as.matrix(cor(t(final_w2vec[[l]])))
  cor_vectors_append <- rbind(cor_vectors_append, cor_final_w2vec[[l]][lower.tri(cor_final_w2vec[[l]])])
}

word_combs <- paste0(word_pairs[1,], "_", word_pairs[2,])

cor_vectors_append[is.na(cor_vectors_append)] <- 0

time_diff <- c(1:10, 10:16) 
time_diff <- time_diff - mean(time_diff)
betahat <- t(cor_vectors_append) %*% time_diff
cor_filtered <- cor_vectors_append[, order(abs(betahat), decreasing = TRUE)[1:20000]]
colnames(cor_filtered) <- word_combs[order(abs(betahat), decreasing = TRUE)[1:20000]]

#save(cor_filtered, file = "../output/cor_filtered_words_bl_nd.rda")

cor_filtered <- get(load("../output/cor_filtered_words_bl_nd.rda"))


epoch <- time_diff
betahat <- array(0, dim(cor_filtered)[2])
shat <- array(0, dim(cor_filtered)[2])

for(l in 1:length(betahat)){
  na_idx <- which(is.na(temp))
  if(length(na_idx) > 2){
    betahat[l] <- 0
    shat[l] <- 0
  }else{
    betahat[l] <- coefficients(summary(lm(cor_filtered[,l] ~ epoch)))[2,1]
    shat[l] <- coefficients(summary(lm(cor_filtered[,l] ~ epoch)))[2,2]
  }
}
zscore <- betahat/shat

significant_calls <- colnames(cor_filtered)[which(abs(zscore) > 5.8)]
zscore_sig_calls <- zscore[which(abs(zscore) > 5.8)]

significant_calls_neg <- significant_calls[which(zscore_sig_calls < 0)]
significant_calls_neg <- significant_calls_neg[order(abs(zscore_sig_calls[zscore_sig_calls < 0]), decreasing = TRUE)]

significant_calls_pos <- significant_calls[which(zscore_sig_calls > 0)]
significant_calls_pos <- significant_calls_pos[order(abs(zscore_sig_calls[zscore_sig_calls > 0]), decreasing = TRUE)]


write.table(significant_calls_pos, "../output/bl_nd_increasing_word_assoc.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)
write.table(significant_calls_neg, "../output/bl_nd_decreasing_word_assoc.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)


epoch2 <- factor(c(rep(1,8), rep(2,9)))

betahat2 <- array(0, dim(cor_filtered)[2])
shat2 <- array(0, dim(cor_filtered)[2])

for(l in 1:length(betahat)){
  temp <- cor_filtered[,l]
  na_idx <- which(is.na(temp))
  if(length(unique(epoch2[na_idx])) == 1){
    betahat2[l] <- 0
    shat2[l] <- 0
  }
  else if(length(na_idx) > 2){
    betahat2[l] <- 0
    shat2[l] <- 0
  }else{
    betahat2[l] <- coefficients(summary(lm(cor_filtered[,l] ~ factor(epoch2))))[2,1]
    shat2[l] <- coefficients(summary(lm(cor_filtered[,l] ~ factor(epoch2))))[2,2]
  }
}
zscore2 <- betahat2/shat2



significant_calls <- colnames(cor_filtered)[which(abs(zscore2) > 5.7)]
zscore_sig_calls <- zscore2[which(abs(zscore2) > 5.7)]

significant_calls_neg <- significant_calls[which(zscore_sig_calls < 0)]
significant_calls_neg <- significant_calls_neg[order(abs(zscore_sig_calls[zscore_sig_calls < 0]), decreasing = TRUE)]

significant_calls_pos <- significant_calls[which(zscore_sig_calls > 0)]
significant_calls_pos <- significant_calls_pos[order(abs(zscore_sig_calls[zscore_sig_calls > 0]), decreasing = TRUE)]

write.table(significant_calls_pos, "../output/bl_nd_increasing_word_assoc_2.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)
write.table(significant_calls_neg, "../output/bl_nd_decreasing_word_assoc_2.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)

