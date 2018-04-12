

################   Negro analysis  Ebony   ########################

library(wordVectors)
dirs <- c("../Ebony/1961/", "../Ebony/1962/", "../Ebony/1963/",
          "../Ebony/1964/", "../Ebony/1965/", "../Ebony/1966/",
          "../Ebony/1967/", "../Ebony/1968/", "../Ebony/1969/",
          "../Ebony/1970/", "../Ebony/1971/", "../Ebony/1972/",
          "../Ebony/1973/", "../Ebony/1974/", "../Ebony/1975/",
          "../Ebony/1976/")

model_list <- list()
for(m in 1:length(dirs)){
  model_list[[m]] = read.vectors(paste0(dirs[m], "pooled_word2vec.bin"))
}

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

for(m in 1:length(dirs)){
  
  tmp_similar_1 <- c(tmp_similar_1, 
                     cosineSimilarity(model_list[[m]][[c("negro", "negroes", "negros"), average = TRUE]], 
                                      model_list[[m]][[c("struggle"),
                                             average = TRUE]]))
  tmp_similar_2 <- c(tmp_similar_2, 
                     cosineSimilarity(model_list[[m]][[c("negro", "negroes", "negros"), average = TRUE]], 
                                      model_list[[m]][[c("revolution",  "movement"),
                                             average = TRUE]]))
  
  
  tmp_similar_3 <- c(tmp_similar_3, 
                     cosineSimilarity(model_list[[m]][[c("negro", "negroes", "negros"), average = TRUE]], 
                                      model_list[[m]][[c("protests", "protest", "revolution", 
                                               "struggle", "freedom", "movement",
                                               "violence", "police"),
                                             average = TRUE]]))
  
  tmp_similar_4 <- c(tmp_similar_4, 
                     cosineSimilarity(model_list[[m]][[c("negro", "negroes", "negros"), average = TRUE]], 
                                      model_list[[m]][[c("black", "blacks"),
                                             average = TRUE]]))
  
  
  tmp_similar_5 <- c(tmp_similar_5, 
                     cosineSimilarity(model_list[[m]][[c("negro", "negroes", "negros", "black", "blacks"), average = TRUE]], 
                                      model_list[[m]][[c("poet", "poets", "poetry"),
                                             average = TRUE]]))
  
  tmp_similar_6 <- c(tmp_similar_6, 
                     cosineSimilarity(model_list[[m]][[c("black", "blacks"), average = TRUE]], 
                                      model_list[[m]][[c("art", "arts"),
                                             average = TRUE]]))
  
  tmp_similar_7 <- c(tmp_similar_7, 
                     cosineSimilarity(model_list[[m]][[c("negro", "negroes", "negros"), average = TRUE]], 
                                      model_list[[m]][[c("art", "arts"),
                                             average = TRUE]]))
  
  tmp_similar_8 <- c(tmp_similar_8, 
                     cosineSimilarity(model_list[[m]][[c("black"), average = TRUE]], 
                                      model_list[[m]][[c("power"),
                                             average = TRUE]]))
  tmp_similar_9 <- c(tmp_similar_9, 
                      cosineSimilarity(model_list[[m]][[c("stokely"), average = TRUE]], 
                                       model_list[[m]][[c("carmichael"),
                                              average = TRUE]]))
  
  tmp_similar_10 <- c(tmp_similar_10, 
                      cosineSimilarity(model_list[[m]][[c("black"), average = TRUE]], 
                                       model_list[[m]][[c("panther", "panthers"),
                                              average = TRUE]]))
  
  
}

df <- data.frame("year" =  c(1961:1976),
                 "score" = tmp_similar_1)
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +  
            geom_smooth(method = "lm", size = 1.5) +
            ggtitle("negro vs struggle trend plot")
p

df <- data.frame("year" =  c(1961:1976),
                 "score" = tmp_similar_2)
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +  
           geom_smooth(method = "lm", size = 1.5) +
           ggtitle("negro vs (movement, revolution) trend plot")
p

df <- data.frame("year" =  c(1961:1976),
                 "score" = tmp_similar_3)
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +  
            geom_smooth(method = "lm", size = 1.5) +
            ggtitle("negro vs protest terms trend plot")
p

df <- data.frame("year" =  c(1961:1976),
                 "score" = tmp_similar_4)
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +  
          geom_smooth(method = "lm", size = 1.5) +
          ggtitle("negro vs black trend plot")
p

df <- data.frame("year" =  c(1961:1976),
                 "score" = tmp_similar_5)
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +  
         geom_smooth(method = "lm", size = 1.5) +
         ggtitle("(black, negro) vs poetry trend plot")
p

df <- data.frame("year" =  c(1961:1976),
                 "score" = tmp_similar_6)
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +  
             geom_smooth(method = "lm", size = 1.5) +
             ggtitle("black vs arts trend plot")
p

df <- data.frame("year" =  c(1961:1976),
                 "score" = tmp_similar_7)
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +  
         geom_smooth(method = "lm", size = 1.5) +
         ggtitle("negro vs arts trend plot")
p

df <- data.frame("year" =  c(1961:1976),
                 "score" = tmp_similar_8)
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +  
         geom_smooth(method = "lm", size = 1.5) +
         ggtitle("black vs power trend plot")
p

df <- data.frame("year" =  c(1961:1976),
                 "score" = tmp_similar_9)
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") + 
                    ggtitle("stokely vs carmichael trend plot")
p

df <- data.frame("year" =  c(1961:1976),
                 "score" = tmp_similar_10)
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") + 
                    ggtitle("black vs panther trend plot")
p
