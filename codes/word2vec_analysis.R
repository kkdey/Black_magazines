
#########   applying word2vec analysis  across years   ####################


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

for(m in 1:length(dirs)){
  if (!file.exists(paste0(dirs[m], "pooled_word2vec.bin"))){
    model = train_word2vec(paste0(dirs[m], "pooled.txt"),
                           paste0(dirs[m], "/Users/Guest/Downloads/pooled_word2vec.bin"),
                           vectors=200,threads=4,window=10,
                           iter=50,negative_samples=0)
  }else{
    model = read.vectors(paste0(dirs[m], "pooled_word2vec.bin"))
  }
}

tmp_negro <- list()
tmp_black <- list()
tmp_civil <- list()

tmp_similar_1 <- c()
tmp_similar_2 <- c()
tmp_similar_3 <- c()
tmp_similar_31 <- c()
tmp_similar_4 <- c()
tmp_similar_5 <- c()
tmp_similar_6 <- c()
tmp_similar_7 <- c()
tmp_similar_8 <- c()
tmp_similar_9 <- c()
tmp_similar_10 <- c()
tmp_similar_11 <- c()
for(m in 1:length(dirs)){
  
  model = read.vectors(paste0(dirs[m], "pooled_word2vec.bin"))
  tmp_negro[[m]] <- model %>% closest_to(c("negro", "negroes", "negros"), n = 20)
  tmp_black[[m]] <- model %>% closest_to(c("black", "blacks"), n = 20)
  tmp_civil[[m]] <- model %>% closest_to(c("civil", "rights"), n = 20)
  
  tmp_similar_1 <- c(tmp_similar_1, 
                   cosineSimilarity(model[[c("negro", "negroes", "negros"), average = TRUE]], 
                                    model[[c("protests", "protest"),
                                           average = TRUE]]))
  tmp_similar_2 <- c(tmp_similar_2, 
                   cosineSimilarity(model[[c("negro", "negroes", "negros"), average = TRUE]], 
                                    model[[c("revolution",  "movement"),
                                           average = TRUE]]))
  
  tmp_similar_3 <- c(tmp_similar_3, 
                     cosineSimilarity(model[[c("negro", "negroes", "negros"), average = TRUE]], 
                                      model[[c("struggle"),
                                             average = TRUE]]))
  tmp_similar_31 <- c(tmp_similar_31, 
                     cosineSimilarity(model[[c("negro", "negroes", "negros"), average = TRUE]], 
                                      model[[c("police"),
                                             average = TRUE]]))
  
  tmp_similar_4 <- c(tmp_similar_4, 
                     cosineSimilarity(model[[c("negro", "negroes", "negros"), average = TRUE]], 
                                      model[[c("slave", "slaves", "slavery"),
                                             average = TRUE]]))
  tmp_similar_5 <- c(tmp_similar_5, 
                     cosineSimilarity(model[[c("negro", "negroes", "negros"), average = TRUE]], 
                                      model[[c("violence"),
                                             average = TRUE]]))
  
  
  tmp_similar_6 <- c(tmp_similar_6, 
                     cosineSimilarity(model[[c("negro", "negroes", "negros"), average = TRUE]], 
                                      model[[c("protests", "protest", "revolution", 
                                               "struggle", "freedom", "movement",
                                               "violence", "police"),
                                             average = TRUE]]))
  
  tmp_similar_7 <- c(tmp_similar_7, 
                     cosineSimilarity(model[[c("negro", "negroes", "negros"), average = TRUE]], 
                                      model[[c("black", "blacks"),
                                             average = TRUE]]))
  
  tmp_similar_8 <- c(tmp_similar_8, 
                     cosineSimilarity(model[[c("negro", "negroes", "negros"), average = TRUE]], 
                                      model[[c("art", "arts"),
                                             average = TRUE]]))
  
  tmp_similar_9 <- c(tmp_similar_9, 
                     cosineSimilarity(model[[c("negro", "negroes", "negros"), average = TRUE]], 
                                      model[[c("music", "song", "songs"),
                                             average = TRUE]]))
  tmp_similar_10 <- c(tmp_similar_10, 
                     cosineSimilarity(model[[c("negro", "negroes", "negros"), average = TRUE]], 
                                      model[[c("poet", "poets", "poetry"),
                                             average = TRUE]]))
  
  tmp_similar_11 <- c(tmp_similar_11, 
                     cosineSimilarity(model[[c("negro", "negroes", "negros"), average = TRUE]], 
                                      model[[c("theater", "drama"),
                                             average = TRUE]]))
  
}

tmp_race <- list()
tmp_africa <- list()
tmp_similar_12 <- c()

for(m in 1:length(dirs)){
  
  model = read.vectors(paste0(dirs[m], "pooled_word2vec.bin"))
  tmp_race[[m]] <- model %>% closest_to(c("race", "racial", "racism", "racially", "racist"), n = 20)
  tmp_africa[[m]] <- model %>% closest_to(c("african", "afrikan"), n = 20)
  tmp_similar_12 <- c(tmp_similar_12, 
                      cosineSimilarity(model[[c("african"), average = TRUE]], 
                                       model[[c("american"),
                                              average = TRUE]]))
}


df <- data.frame("year" =  c(1961:1970,  1970:1976),
                 "score" = tmp_similar_6)
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +  geom_smooth(method = "loess", size = 1.5)
p

tmp_similar_3
df <- data.frame("year" =  c(1961:1970,  1970:1976),
                 "score" = tmp_similar_11)
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +  geom_smooth(method = "loess", size = 1.5)
p


