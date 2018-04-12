

############  politics ebony analysis  #################


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


tmp_king <- list()
tmp_panther <- list()
tmp_power <- list()

for(m in 1:length(dirs)){
  tmp_king[[m]] <- model_list[[m]] %>% closest_to(c("martin_luther_king", "martin_luther_kings",
                                                    "dr_king", "dr_kings"), n = 30)
  tmp_panther[[m]] <- model_list[[m]] %>% closest_to(c("black_panther", "black_panthers"), n = 30)
  tmp_power[[m]] <- model_list[[m]] %>% closest_to(c("black_power"), n = 30)
}

tmp_similar <- vector(mode = "list", length = 6)
for(i in 1:1){
  tmp_similar[[i]] <- as.numeric()
}

for(m in 1:length(dirs)){
  tmp_similar[[1]] <- c(tmp_similar[[1]],
                        cosineSimilarity(model_list[[m]][[c("martin_luther_king", "martin_luther_kings",
                                                            "dr_king", "dr_kings"), average = TRUE]],
                                         model_list[[m]][[c("assassination", "death", "died", "murder",
                                                            "assassinated", "murdered"),
                                                          average = TRUE]]))
  
  tmp_similar[[2]] <- c(tmp_similar[[2]],
                        cosineSimilarity(model_list[[m]][[c("black_power"), average = TRUE]],
                                         model_list[[m]][[c("stokely", "carmichael"),
                                                          average = TRUE]]))
  
  tmp_similar[[3]] <- c(tmp_similar[[3]],
                        cosineSimilarity(model_list[[m]][[c("black_panther", "black_panthers"), average = TRUE]],
                                         model_list[[m]][[c("stokely", "carmichael"),
                                                          average = TRUE]]))
  
  
}




df <- data.frame("year" =  c(1961:1976),
                 "score" = tmp_similar[[1]])
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +
  geom_smooth(method = "lm", size = 1.5) + ggtitle("dr.king vs (assassination)")
p

df <- data.frame("year" =  c(1961:1976),
                 "score" = tmp_similar[[2]])
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +
  geom_smooth(method = "lm", size = 1.5) + ggtitle("black_power vs (stokely)")
p

df <- data.frame("year" =  c(1961:1976),
                 "score" = tmp_similar[[3]])
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +
  geom_smooth(method = "lm", size = 1.5) + ggtitle("black_power vs (stokely)")
p



