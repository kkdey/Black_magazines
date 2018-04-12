
####################### all word2vec similarities  ###############################

############  Ebony  #################

dir <- list.files("../Ebony/")
library(wordVectors)
w2vec <- list()
words <- c()
for(l in 1:16){
  w2vec[[l]] <- read.vectors(paste0("../Ebony/", dir[l], "/pooled_word2vec.bin"))
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
for(l in 1:16){
  final_w2vec[[l]] <- w2vec[[l]][match(words_filtered, rownames(w2vec[[l]])),]
  cor_final_w2vec[[l]] <- as.matrix(cor(t(final_w2vec[[l]])))
  cor_vectors_append <- rbind(cor_vectors_append, cor_final_w2vec[[l]][lower.tri(cor_final_w2vec[[l]])])
}

word_combs <- paste0(word_pairs[1,], "_", word_pairs[2,])

colnames(cor_vectors_append) <- word_combs

save(cor_vectors_append, file = "../output/cor_vectors_append_ebony.rda")

time_diff <- 1:16 - mean(1:16)

cor_vectors_append[is.na(cor_vectors_append)] <- 0
betahat <- t(cor_vectors_append) %*% time_diff




#save(betahat, file = "../output/betahat_ebony.rda")

betahat <- get(load("../output/betahat_ebony.rda"))

word_combs[order(betahat, decreasing = TRUE)[1:100]]


cor_filtered <- cor_vectors_append[, order(abs(betahat), decreasing = TRUE)[1:20000]]
colnames(cor_filtered) <- word_combs[order(abs(betahat), decreasing = TRUE)[1:20000]]

#save(cor_filtered, file = "../output/cor_filtered_words_ebony.rda")

cor_filtered <- get(load("../output/cor_filtered_words_ebony.rda"))

#############  run linear regression  #########################

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

betahat3 <- t(cor_filtered) %*% time_diff

epoch2 <- factor(c(rep(1,8), rep(2,8)))

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




significant_calls <- colnames(cor_filtered)[which(abs(zscore2) > 6)]
zscore_sig_calls <- zscore2[which(abs(zscore2) > 6)]

significant_calls_neg <- significant_calls[which(zscore_sig_calls < 0)]
significant_calls_neg <- significant_calls_neg[order(abs(zscore_sig_calls[zscore_sig_calls < 0]), decreasing = TRUE)]

significant_calls_pos <- significant_calls[which(zscore_sig_calls > 0)]
significant_calls_pos <- significant_calls_pos[order(abs(zscore_sig_calls[zscore_sig_calls > 0]), decreasing = TRUE)]

plot(cor_filtered[, significant_calls_neg[1]])
plot(cor_filtered[, significant_calls_pos[29]])



significant_calls <- colnames(cor_filtered)[which(abs(zscore) > 6)]
zscore_sig_calls <- zscore[which(abs(zscore) > 6)]

significant_calls_neg <- significant_calls[which(zscore_sig_calls < 0)]
significant_calls_neg <- significant_calls_neg[order(abs(zscore_sig_calls[zscore_sig_calls < 0]), decreasing = TRUE)]

significant_calls_pos <- significant_calls[which(zscore_sig_calls > 0)]
significant_calls_pos <- significant_calls_pos[order(abs(zscore_sig_calls[zscore_sig_calls > 0]), decreasing = TRUE)]

plot(cor_filtered[, significant_calls_neg[1]])
plot(cor_filtered[, significant_calls_pos[29]])



colnames(cor_filtered)[order(zscore, decreasing = TRUE)[1:50]]
colnames(cor_filtered)[order(zscore, decreasing = FALSE)[1:50]]

colnames(cor_filtered)[order(zscore2, decreasing = TRUE)[1:50]]
colnames(cor_filtered)[order(zscore2, decreasing = FALSE)[1:50]]

zscore2[order(zscore2, decreasing = TRUE)[1:300]]

plot(cor_filtered[,order(zscore2, decreasing = TRUE)[53]])
plot(cor_filtered[,order(zscore2, decreasing = TRUE)[1]])

plot(cor_filtered[,order(zscore2, decreasing = FALSE)[267]])

word_frequencies <- get(load("../output/table_word_frequencies_ebony.rda"))


plot(cor_filtered[,"estimate_mortality"])

plot(cor_filtered[,"dolls_profit"])





save(cor_vectors_append, file = "../output/cor_vectors_append_ebony.rda")

cor_vectors_append <- get(load(file = "../output/cor_vectors_append_ebony.rda"))

