

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


model_list <- list()
for(m in 1:length(dirs)){
  model_list[[m]] = read.vectors(paste0(dirs[m], "pooled_word2vec.bin"))
}


tmp_black <- list()
tmp_arts <- list()
arts <- c("arts", "art", "music", "musicians", "musician",
   "poem", "poems", "poet", "poets",
   "poetry", "literature", "novels",
   "fiction", "theatre", "theater",
   "blues", "jazz", "magazines",
   "magazine", "painting", "sculpture", "painter", "artist", "artists",
   "sculpture", "composer", "prose", "ballads", "anthology" )


for(m in 1:length(dirs)){
  tmp_black[[m]] <- model_list[[m]] %>% closest_to(c("black", "blacks"), n = 100)
  tmp_arts[[m]] <- model_list[[m]] %>% closest_to(arts, n = 100)
}





# words <- c("arts", "art", "music", 
#   "poem", "poems", "poet", "poets",
#   "poetry", "literature", "novels",
#   "fiction", "theatre", "theater",
#   "blues", "jazz")

words <- c("jazz", "blues")

words <- c("slave", "slaves")

words <- arts

tmp_similar <- vector(mode = "list", length = 6)
for(i in 1:6){
  tmp_similar[[i]] <- as.numeric()
}

for(m in 1:length(dirs)){
  tmp_similar[[1]] <- c(tmp_similar[[1]],
                        cosineSimilarity(model_list[[m]][[words, average = TRUE]],
                                         model_list[[m]][[c("blacks", "black"),
                                                          average = TRUE]]))
  tmp_similar[[2]] <- c(tmp_similar[[2]],
                        cosineSimilarity(model_list[[m]][[words, average = TRUE]],
                                         model_list[[m]][[c("negro", "negros", "negroes"),
                                                          average = TRUE]]))
  tmp_similar[[3]] <- c(tmp_similar[[3]],
                        cosineSimilarity(model_list[[m]][[words, average = TRUE]],
                                         model_list[[m]][[c("negro", "negros", "negroes",
                                                            "black", "blacks"),
                                                          average = TRUE]]))
}

df <- data.frame("year" =  c(1961:1970,  1970:1976),
                 "score" = tmp_similar[[1]])
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +
  geom_smooth(method = "lm", size = 1.5) + ggtitle("arts vs (black, blacks)")
p

df <- data.frame("year" =  c(1961:1970,  1970:1976),
                 "score" = tmp_similar[[2]])
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +
  geom_smooth(method = "lm", size = 1.5) + ggtitle("arts vs (negros, blacks)")
p

df <- data.frame("year" =  c(1961:1970,  1970:1976),
                 "score" = tmp_similar[[3]])
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +
  geom_smooth(method = "lm", size = 1.5) + ggtitle("arts vs (negro, negros)")
p


###############################   Ebony articles   #######################################


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

tmp_black <- list()
tmp_arts <- list()
arts <- c("arts", "art", "music", "musicians", "musician",
          "poem", "poems", "poet", "poets",
          "poetry", "literature", "novels",
          "fiction", "theatre", "theater",
          "blues", "jazz", "magazines",
          "magazine", "painting", "sculpture", "painter", "artist", "artists",
          "sculpture", "composer", "prose", "ballads", "anthology" )


for(m in 1:length(dirs)){
  tmp_black[[m]] <- model_list[[m]] %>% closest_to(c("black", "blacks"), n = 100)
  tmp_arts[[m]] <- model_list[[m]] %>% closest_to(arts, n = 100)
}

#words <- c("jazz", "blues")

words <- c("slave", "slaves")


#words <- arts

tmp_similar <- vector(mode = "list", length = 6)
for(i in 1:6){
  tmp_similar[[i]] <- as.numeric()
}

for(m in 1:length(dirs)){
  tmp_similar[[1]] <- c(tmp_similar[[1]],
                        cosineSimilarity(model_list[[m]][[words, average = TRUE]],
                                         model_list[[m]][[c("blacks", "black"),
                                                          average = TRUE]]))
  tmp_similar[[2]] <- c(tmp_similar[[2]],
                        cosineSimilarity(model_list[[m]][[words, average = TRUE]],
                                         model_list[[m]][[c("negro", "negros", "negroes"),
                                                          average = TRUE]]))
  tmp_similar[[3]] <- c(tmp_similar[[3]],
                        cosineSimilarity(model_list[[m]][[words, average = TRUE]],
                                         model_list[[m]][[c("negro", "negros", "negroes",
                                                            "black", "blacks"),
                                                          average = TRUE]]))
}

df <- data.frame("year" =  c(1961:1976),
                 "score" = tmp_similar[[1]])
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +
  geom_smooth(method = "lm", size = 1.5) + ggtitle("arts vs (black, blacks)")
p

df <- data.frame("year" =  c(1961:1976),
                 "score" = tmp_similar[[2]])
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +
  geom_smooth(method = "lm", size = 1.5) + ggtitle("arts vs (negros, blacks)")
p

df <- data.frame("year" =  c(1961:1976),
                 "score" = tmp_similar[[3]])
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +
  geom_smooth(method = "lm", size = 1.5) + ggtitle("arts vs (negro, negros)")
p

