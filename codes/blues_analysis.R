

##########  Blues in the magazine   ##################

library(wordVectors)
dirs <- c("../Negro_Digest/ND_1961/",
          "../Negro_Digest/ND_1962/",
          "../Negro_Digest/ND_1963/",
          "../Negro_Digest/ND_1964/",
          "../Negro_Digest/ND_1965/",
          "../Negro_Digest/ND_1966/",
          "../Negro_Digest/ND_1967/",
          "../Negro_Digest/ND_1968/",
          "../Negro_Digest/ND_1969/",
          "../Negro_Digest/ND_1970/",
          "../Black_World/BL_1970/",
          "../Black_World/BL_1971/",
          "../Black_World/BL_1972/",
          "../Black_World/BL_1973/",
          "../Black_World/BL_1974/",
          "../Black_World/BL_1975/",
          "../Black_World/BL_1976/")

tab <- get(load("../output/table_word_frequencies.rda"))
tab2 <- t(apply(tab, 1, function(x) return(x/sum(x))))
plot(tab2[, "jazz"])

plot(tab2[, "jazz"], col = "red", pch = 20, cex = 1.5, ylab = "", xlab = "", xaxt = "n")
points(tab2[, "blues"], col = "blue", pch = 20, cex = 1.5)
axis(1, at = 1:17, c(paste0("ND_", 1961:1970), paste0("BL_", 1970:1976)), las = 2, cex = 0.7)
legend ("topright", legend=c("jazz", "blues"), col=c("red", "blue"), pch = c(20,20), cex = 0.5)
title("Trend of jazz and blues across time")

tmp_blues <- list()

for(m in 1:length(dirs)){
  model = read.vectors(paste0(dirs[m], "pooled_word2vec.bin"))
  tmp_blues[[m]] <- model %>% closest_to(c("blues"), n = 20)
}

tmp_similar_1 <- c()
for(m in 1:length(dirs)){
  model = read.vectors(paste0(dirs[m], "pooled_word2vec.bin"))
  tmp_similar_1 <- c(tmp_similar_1, 
                     cosineSimilarity(model[[c("blues"), average = TRUE]], 
                                      model[[c("jazz"),
                                             average = TRUE]]))
}

plot(tmp_similar_1)


  
