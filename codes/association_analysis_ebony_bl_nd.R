

###############  Association Analysis (Ebony and Black World/ Negro Digest)  ##########################

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

for(m in 1:length(dirs)){
  
  tmp_similar_1 <- c(tmp_similar_1, 
                     cosineSimilarity(model_list[[m]][[c("negro", "negroes", "negros"), average = TRUE]], 
                                      model_list[[m]][[c("struggle"),
                                                       average = TRUE]]))
  tmp_similar_2 <- c(tmp_similar_2, 
                     cosineSimilarity(model_list[[m]][[c("martin_luther_king", "martin_luther_kings", 
                                                         "dr_king", "dr_kings"), average = TRUE]], 
                                      model_list[[m]][[c("assassination",  "assassinated", "death", "died",
                                                         "murder"),
                                                       average = TRUE]]))
  
  tmp_similar_3 <- c(tmp_similar_3, 
                     cosineSimilarity(model_list[[m]][[c("negro", "negroes", "negros"), average = TRUE]], 
                                      model_list[[m]][[c("black", "blacks"),
                                                       average = TRUE]]))
  
  tmp_similar_4 <- c(tmp_similar_4, 
                     cosineSimilarity(model_list[[m]][[c("negro", "negroes", "negros", "black", "blacks"), average = TRUE]], 
                                      model_list[[m]][[c("poet", "poets", "poetry", "poem", "poems"),
                                                       average = TRUE]]))
  
  tmp_similar_5 <- c(tmp_similar_5, 
                     cosineSimilarity(model_list[[m]][[c("civil_rights"), average = TRUE]], 
                                      model_list[[m]][[c("discrimination", "race"),
                                                       average = TRUE]]))
  
  
  tmp_similar_6 <- c(tmp_similar_6, 
                      cosineSimilarity(model_list[[m]][[c("black_power"), average = TRUE]], 
                                       model_list[[m]][[c("black_panther", "black_panthers"),
                                                        average = TRUE]]))
  tmp_similar_7 <- c(tmp_similar_7, 
                     cosineSimilarity(model_list[[m]][[c("afroamerican", "africanamerican"), average = TRUE]], 
                                      model_list[[m]][[c("studies", "literature", "poets", "poet",
                                                         "poetry", "music", "institute"),
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
  ggtitle("dr. king vs assassination trend plot")
p

df <- data.frame("year" =  c(1961:1976),
                 "score" = tmp_similar_3)
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +  
  geom_smooth(method = "lm", size = 1.5) +
  ggtitle("negro vs black trend plot")
p

df <- data.frame("year" =  c(1961:1976),
                 "score" = tmp_similar_4)
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +  
  geom_smooth(method = "lm", size = 1.5) +
  ggtitle("negro vs poetry trend plot")
p

df <- data.frame("year" =  c(1961:1976),
                 "score" = tmp_similar_5)
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +  
  geom_smooth(method = "lm", size = 1.5) +
  ggtitle("civil rights vs race trend plot")
p

df <- data.frame("year" =  c(1961:1976),
                 "score" = tmp_similar_6)
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +  
  geom_smooth(method = "lm", size = 1.5) +
  ggtitle("black power vs black panther trend plot")
p

df <- data.frame("year" =  c(1961:1976),
                 "score" = tmp_similar_7)
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +  
  geom_smooth(method = "lm", size = 1.5) +
  ggtitle("afroamerican vs culture trend plot")
p
