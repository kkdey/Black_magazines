

################  word2vec similarities with all black and negro terms ################

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
words_filtered <- names(num_words[which(num_words > 5)])

cor_black_assoc <- c()
cor_blacks_assoc <- c()
cor_negro_assoc <- c()
cor_negros_assoc <- c()

for(l in 1:16){
  fit <- w2vec[[l]][match(words_filtered, rownames(w2vec[[l]])),]
  rownames(fit) <- words_filtered
  cor <- as.matrix(cor(t(fit), use = "pairwise.complete.obs", method = "pearson"))
  cor_black_assoc <- rbind(cor_black_assoc, cor["black",])
  cor_blacks_assoc <- rbind(cor_blacks_assoc, cor["blacks",])
  cor_negro_assoc <- rbind(cor_negro_assoc, cor["negro",])
  cor_negros_assoc <- rbind(cor_negros_assoc, pmax(cor["negros",], cor["negroes",], na.rm = TRUE))
  cat("We are at year", l, "\n")
}

ll <- list("cor_black_assoc" = cor_black_assoc,
           "cor_blacks_assoc" =  cor_blacks_assoc,
           "cor_negro_assoc" = cor_negro_assoc,
           "cor_negros_assoc" =  cor_negros_assoc)

save(ll, file = "../output/black_negro_assoc_analysis.rda")

################  black   ########################

time_diff <- 1:16 - mean(1:16)
epoch <- time_diff
betahat <- array(0, dim(ll$cor_black_assoc)[2])
shat <- array(0, dim(ll$cor_black_assoc)[2])

for(l in 1:length(betahat)){
  temp <- ll$cor_black_assoc[,l]
  na_idx <- which(is.na(temp))
  if(length(na_idx) > 2){
    betahat[l] <- 0
    shat[l] <- 0
  }else{
    ss <- summary(lm(ll$cor_black_assoc[,l] ~ epoch))
    betahat[l] <- coefficients(ss)[2,1]
    shat[l] <- coefficients(ss)[2,2]
  }
}

epoch2 <- factor(c(rep(1,8), rep(2,8)))

betahat2 <- array(0, dim(ll$cor_black_assoc)[2])
shat2 <- array(0, dim(ll$cor_black_assoc)[2])

for(l in 1:length(betahat)){
  temp <- ll$cor_black_assoc[,l]
  na_idx <- which(is.na(temp))
  if(length(unique(epoch2[na_idx])) == 1){
    betahat2[l] <- 0
    shat2[l] <- 0
  }
  else if(length(na_idx) > 2){
    betahat2[l] <- 0
    shat2[l] <- 0
  }else{
    ss <- summary(lm(ll$cor_black_assoc[,l] ~ factor(epoch2)))
    betahat2[l] <- coefficients(ss)[2,1]
    shat2[l] <- coefficients(ss)[2,2]
  }
}

df <- data.frame("beta" = betahat, "sebeta" = shat, "beta_fac" = betahat2, "sebeta_fac" = shat2)
rownames(df) <- words_filtered

save(df, file = "../output/black_words_assoc.rda")



####################   blacks  ##############################

time_diff <- 1:16 - mean(1:16)
epoch <- time_diff
betahat <- array(0, dim(ll$cor_blacks_assoc)[2])
shat <- array(0, dim(ll$cor_blacks_assoc)[2])

for(l in 1:length(betahat)){
  temp <- ll$cor_blacks_assoc[,l]
  na_idx <- which(is.na(temp))
  if(length(na_idx) > 2){
    betahat[l] <- 0
    shat[l] <- 0
  }else{
    ss <- summary(lm(ll$cor_blacks_assoc[,l] ~ epoch))
    betahat[l] <- coefficients(ss)[2,1]
    shat[l] <- coefficients(ss)[2,2]
  }
}

epoch2 <- factor(c(rep(1,8), rep(2,8)))

betahat2 <- array(0, dim(ll$cor_blacks_assoc)[2])
shat2 <- array(0, dim(ll$cor_blacks_assoc)[2])

for(l in 1:length(betahat)){
  temp <- ll$cor_blacks_assoc[,l]
  na_idx <- which(is.na(temp))
  if(length(unique(epoch2[na_idx])) == 1){
    betahat2[l] <- 0
    shat2[l] <- 0
  }
  else if(length(na_idx) > 2){
    betahat2[l] <- 0
    shat2[l] <- 0
  }else{
    ss <- summary(lm(ll$cor_blacks_assoc[,l] ~ factor(epoch2)))
    betahat2[l] <- coefficients(ss)[2,1]
    shat2[l] <- coefficients(ss)[2,2]
  }
}

df <- data.frame("beta" = betahat, "sebeta" = shat, "beta_fac" = betahat2, "sebeta_fac" = shat2)
rownames(df) <- words_filtered

save(df, file = "../output/blacks_words_assoc.rda")

#################   negro   ###############################


time_diff <- 1:16 - mean(1:16)
epoch <- time_diff
betahat <- array(0, dim(ll$cor_negro_assoc)[2])
shat <- array(0, dim(ll$cor_negro_assoc)[2])

for(l in 1:length(betahat)){
  temp <- ll$cor_negro_assoc[,l]
  na_idx <- which(is.na(temp))
  if(length(na_idx) > 2){
    betahat[l] <- 0
    shat[l] <- 0
  }else{
    ss <- summary(lm(ll$cor_negro_assoc[,l] ~ epoch))
    betahat[l] <- coefficients(ss)[2,1]
    shat[l] <- coefficients(ss)[2,2]
  }
}

epoch2 <- factor(c(rep(1,8), rep(2,8)))

betahat2 <- array(0, dim(ll$cor_negro_assoc)[2])
shat2 <- array(0, dim(ll$cor_negro_assoc)[2])

for(l in 1:length(betahat)){
  temp <- ll$cor_negro_assoc[,l]
  na_idx <- which(is.na(temp))
  if(length(unique(epoch2[na_idx])) == 1){
    betahat2[l] <- 0
    shat2[l] <- 0
  }
  else if(length(na_idx) > 2){
    betahat2[l] <- 0
    shat2[l] <- 0
  }else{
    ss <- summary(lm(ll$cor_negro_assoc[,l] ~ factor(epoch2)))
    betahat2[l] <- coefficients(ss)[2,1]
    shat2[l] <- coefficients(ss)[2,2]
  }
}

df <- data.frame("beta" = betahat, "sebeta" = shat, "beta_fac" = betahat2, "sebeta_fac" = shat2)
rownames(df) <- words_filtered

save(df, file = "../output/negro_words_assoc.rda")


##############  negros  ######################

time_diff <- 1:16 - mean(1:16)
epoch <- time_diff
betahat <- array(0, dim(ll$cor_negros_assoc)[2])
shat <- array(0, dim(ll$cor_negros_assoc)[2])

for(l in 1:length(betahat)){
  temp <- ll$cor_negros_assoc[,l]
  na_idx <- which(is.na(temp))
  if(length(na_idx) > 2){
    betahat[l] <- 0
    shat[l] <- 0
  }else{
    ss <- summary(lm(ll$cor_negros_assoc[,l] ~ epoch))
    betahat[l] <- coefficients(ss)[2,1]
    shat[l] <- coefficients(ss)[2,2]
  }
}

epoch2 <- factor(c(rep(1,8), rep(2,8)))

betahat2 <- array(0, dim(ll$cor_negros_assoc)[2])
shat2 <- array(0, dim(ll$cor_negros_assoc)[2])

for(l in 1:length(betahat)){
  temp <- ll$cor_negros_assoc[,l]
  na_idx <- which(is.na(temp))
  if(length(unique(epoch2[na_idx])) == 1){
    betahat2[l] <- 0
    shat2[l] <- 0
  }
  else if(length(na_idx) > 2){
    betahat2[l] <- 0
    shat2[l] <- 0
  }else{
    ss <- summary(lm(ll$cor_negros_assoc[,l] ~ factor(epoch2)))
    betahat2[l] <- coefficients(ss)[2,1]
    shat2[l] <- coefficients(ss)[2,2]
  }
}

df <- data.frame("beta" = betahat, "sebeta" = shat, "beta_fac" = betahat2, "sebeta_fac" = shat2)
rownames(df) <- words_filtered

save(df, file = "../output/negros_words_assoc.rda")


#####################  Black decreasing trend   #########################

df <- get(load(file = "../output/black_words_assoc.rda"))

z1 <- df[,1]/df[,2]
z1[order(z1, decreasing = TRUE)[1:100]]
rownames(df)[order(z1, decreasing = TRUE)[1:100]]
z2 <- df[,3]/df[,4]
z2[order(z2, decreasing = TRUE)[1:200]]
rownames(df)[order(z2, decreasing = TRUE)[1:100]]


z1 <- df[,1]/df[,2]
z1[order(z1, decreasing = FALSE)[1:200]]
rownames(df)[order(z1, decreasing = FALSE)[1:200]]
z2 <- df[,3]/df[,4]
z2[order(z2, decreasing = FALSE)[1:200]]
rownames(df)[order(z2, decreasing = FALSE)[1:200]]



df <- get(load(file = "../output/blacks_words_assoc.rda"))

z1 <- df[,1]/df[,2]
z1[order(z1, decreasing = TRUE)[1:100]]
rownames(df)[order(z1, decreasing = TRUE)[1:100]]
z2 <- df[,3]/df[,4]
z2[order(z2, decreasing = TRUE)[1:200]]
rownames(df)[order(z2, decreasing = TRUE)[1:100]]


z1 <- df[,1]/df[,2]
z1[order(z1, decreasing = FALSE)[1:200]]
rownames(df)[order(z1, decreasing = FALSE)[1:200]]
z2 <- df[,3]/df[,4]
z2[order(z2, decreasing = FALSE)[1:200]]
rownames(df)[order(z2, decreasing = FALSE)[1:200]]



df <- get(load(file = "../output/negro_words_assoc.rda"))

z1 <- df[,1]/df[,2]
z1[order(z1, decreasing = TRUE)[1:100]]
rownames(df)[order(z1, decreasing = TRUE)[1:100]]
z2 <- df[,3]/df[,4]
z2[order(z2, decreasing = TRUE)[1:200]]
rownames(df)[order(z2, decreasing = TRUE)[1:100]]


z1 <- df[,1]/df[,2]
z1[order(z1, decreasing = FALSE)[1:200]]
rownames(df)[order(z1, decreasing = FALSE)[1:200]]
z2 <- df[,3]/df[,4]
z2[order(z2, decreasing = FALSE)[1:200]]
rownames(df)[order(z2, decreasing = FALSE)[1:200]]


df <- get(load(file = "../output/negros_words_assoc.rda"))

z1 <- df[,1]/df[,2]
z1[order(z1, decreasing = TRUE)[1:100]]
rownames(df)[order(z1, decreasing = TRUE)[1:100]]
z2 <- df[,3]/df[,4]
z2[order(z2, decreasing = TRUE)[1:200]]
rownames(df)[order(z2, decreasing = TRUE)[1:100]]


z1 <- df[,1]/df[,2]
z1[order(z1, decreasing = FALSE)[1:200]]
rownames(df)[order(z1, decreasing = FALSE)[1:200]]
z2 <- df[,3]/df[,4]
z2[order(z2, decreasing = FALSE)[1:200]]
rownames(df)[order(z2, decreasing = FALSE)[1:200]]
