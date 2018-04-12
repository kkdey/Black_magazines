
#################  word2vec analysis  part   2 ###################

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

tmp_similar_1 <- c()
tmp_similar_2 <- c()
tmp_similar_3 <- c()
tmp_similar_4 <- c()
tmp_similar_5 <- c()
tmp_similar_6 <- c()
tmp_similar_7 <- c()
tmp_similar_8 <- c()
tmp_similar_9 <- c()
tmp_similar_10 <- c()
tmp_similar_11 <- c()
tmp_similar_12 <- c()

for(m in 1:length(dirs)){
  
  model = read.vectors(paste0(dirs[m], "pooled_word2vec.bin"))

  tmp_similar_1 <- c(tmp_similar_1, 
                     cosineSimilarity(model[[c("negro", "negroes", "negros", "black", "blacks"), average = TRUE]], 
                                      model[[c("protests", "protest"),
                                             average = TRUE]]))
  tmp_similar_2 <- c(tmp_similar_2, 
                     cosineSimilarity(model[[c("negro", "negroes", "negros"), average = TRUE]], 
                                      model[[c("revolution",  "movement"),
                                             average = TRUE]]))
 
  
  tmp_similar_3 <- c(tmp_similar_3, 
                     cosineSimilarity(model[[c("negro", "negroes", "negros", "black", "blacks"), average = TRUE]], 
                                      model[[c("protests", "protest", "revolution", 
                                               "struggle", "freedom", "movement",
                                               "violence", "police"),
                                             average = TRUE]]))
  
  tmp_similar_4 <- c(tmp_similar_4, 
                     cosineSimilarity(model[[c("negro", "negroes", "negros"), average = TRUE]], 
                                      model[[c("black", "blacks"),
                                             average = TRUE]]))
  
  tmp_similar_5 <- c(tmp_similar_5, 
                     cosineSimilarity(model[[c("negro", "negroes", "negros", "black", "blacks"), average = TRUE]], 
                                      model[[c("art", "arts"),
                                             average = TRUE]]))
  
  tmp_similar_6 <- c(tmp_similar_6, 
                      cosineSimilarity(model[[c("negro", "negroes", "negros", "black", "blacks"), average = TRUE]], 
                                       model[[c("poet", "poets", "poetry"),
                                              average = TRUE]]))
  
  tmp_similar_7 <- c(tmp_similar_7, 
                     cosineSimilarity(model[[c("black", "blacks"), average = TRUE]], 
                                      model[[c("art", "arts"),
                                             average = TRUE]]))
  
  tmp_similar_8 <- c(tmp_similar_8, 
                     cosineSimilarity(model[[c("negro", "negroes", "negros"), average = TRUE]], 
                                      model[[c("art", "arts"),
                                             average = TRUE]]))
  
  tmp_similar_9 <- c(tmp_similar_9, 
                     cosineSimilarity(model[[c("black"), average = TRUE]], 
                                      model[[c("power"),
                                             average = TRUE]]))
  tmp_similar_10 <- c(tmp_similar_10, 
                     cosineSimilarity(model[[c("stokely"), average = TRUE]], 
                                      model[[c("carmichael"),
                                             average = TRUE]]))
  
  tmp_similar_11 <- c(tmp_similar_11, 
                      cosineSimilarity(model[[c("black"), average = TRUE]], 
                                       model[[c("american", "americans"),
                                              average = TRUE]]))
  
  tmp_similar_12 <- c(tmp_similar_12, 
                      cosineSimilarity(model[[c("black"), average = TRUE]], 
                                       model[[c("panther"),
                                              average = TRUE]]))
  
  
}

df <- data.frame("year" =  c(1961:1970,  1970:1976),
                 "score" = tmp_similar_1)
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +  geom_smooth(method = "loess", size = 1.5)
p

df <- data.frame("year" =  c(1961:1970,  1970:1976),
                 "score" = tmp_similar_2)
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +  geom_smooth(method = "loess", size = 1.5)
p

df <- data.frame("year" =  c(1961:1970,  1970:1976),
                 "score" = tmp_similar_3)
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +  geom_smooth(method = "loess", size = 1.5)
p

df <- data.frame("year" =  c(1961:1970,  1970:1976),
                 "score" = tmp_similar_4)
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +  geom_smooth(method = "loess", size = 1.5)
p

df <- data.frame("year" =  c(1961:1970,  1970:1976),
                 "score" = tmp_similar_5)
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +  geom_smooth(method = "loess", size = 1.5)
p

df <- data.frame("year" =  c(1961:1970,  1970:1976),
                 "score" = tmp_similar_6)
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +  geom_smooth(method = "loess", size = 1.5)
p

df <- data.frame("year" =  c(1961:1970,  1970:1976),
                 "score" = tmp_similar_7)
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +  geom_smooth(method = "loess", size = 1.5)
p

df <- data.frame("year" =  c(1961:1970,  1970:1976),
                 "score" = tmp_similar_8)
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +  geom_smooth(method = "loess", size = 1.5)
p

df <- data.frame("year" =  c(1961:1970,  1970:1976),
                 "score" = tmp_similar_9)
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") + ggtitle("association between black and power")
p

df <- data.frame("year" =  c(1961:1970,  1970:1976),
                 "score" = tmp_similar_10)
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") + ggtitle("association between stokely and carmichael")
p

df <- data.frame("year" =  c(1961:1970,  1970:1976),
                 "score" = tmp_similar_11)
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") + ggtitle("association between black and american")
p

df <- data.frame("year" =  c(1961:1970,  1970:1976),
                 "score" = tmp_similar_12)
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") + ggtitle("association between black and panther")
p

tmp_carmichael <- list()
for(m in 1:length(dirs)){
  
  model = read.vectors(paste0(dirs[m], "pooled_word2vec.bin"))
  tmp_carmichael[[m]] <- model %>% closest_to(c("carmichael"), n = 20)
}
