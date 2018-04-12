

#############  Blues analysis  (ebony)  ###################

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

tmp_blues <- list()
tmp_black <- list()
tmp_negro <- list()
tmp_negro_black <- list()
tmp_tones <- list()
tmp_rhythms <- list()
tmp_language <- list()
tmp_feminism <- list()

for(m in 1:length(dirs)){
  tmp_blues[[m]] <- model_list[[m]] %>% closest_to(c("blues"), n = 30)
  tmp_black[[m]] <- model_list[[m]] %>% closest_to(c("black", "blacks"), n = 30)
  tmp_negro[[m]] <- model_list[[m]] %>% closest_to(c("negro", "negros"), n = 30)
  tmp_negro_black[[m]] <- model_list[[m]] %>% closest_to(c("negro", "negros", "black", "blacks"), n = 30)
  tmp_tones[[m]] <- model_list[[m]] %>% closest_to(c("tone", "tones"), n = 30)
  tmp_rhythms[[m]] <- model_list[[m]] %>% closest_to(c("rhythm", "rhythms"), n = 30)
  tmp_language[[m]] <- model_list[[m]] %>% closest_to(c("language"), n = 30)
  tmp_feminism[[m]] <- model_list[[m]] %>% closest_to(c("feminist", "feminism"), n = 30)
}

tmp_similar <- vector(mode = "list", length = 6)
for(i in 1:6){
  tmp_similar[[i]] <- as.numeric()
}

for(m in 1:length(dirs)){
  tmp_similar[[1]] <- c(tmp_similar[[1]],
                        cosineSimilarity(model_list[[m]][[c("blues"), average = TRUE]],
                                         model_list[[m]][[c("blacks", "black"),
                                                          average = TRUE]]))
  tmp_similar[[2]] <- c(tmp_similar[[2]],
                        cosineSimilarity(model_list[[m]][[c("blues"), average = TRUE]],
                                         model_list[[m]][[c("negro", "negros","negroes",
                                                            "black", "blacks"),
                                                          average = TRUE]]))
  tmp_similar[[3]] <- c(tmp_similar[[3]],
                        cosineSimilarity(model_list[[m]][[c("blues"), average = TRUE]],
                                         model_list[[m]][[c("rhythm", "rhythms"),
                                                          average = TRUE]]))
  tmp_similar[[4]] <- c(tmp_similar[[4]],
                        cosineSimilarity(model_list[[m]][[c("blues"), average = TRUE]],
                                         model_list[[m]][[c("negro", "negros", "negroes"),
                                                          average = TRUE]]))
  tmp_similar[[5]] <- c(tmp_similar[[5]],
                        cosineSimilarity(model_list[[m]][[c("blues"), average = TRUE]],
                                         model_list[[m]][[c("language"),
                                                          average = TRUE]]))
  tmp_similar[[6]] <- c(tmp_similar[[6]],
                        cosineSimilarity(model_list[[m]][[c("blues"), average = TRUE]],
                                         model_list[[m]][[c("language", "rhythm", "rhythms", "tone", "tones"),
                                                          average = TRUE]]))
}

df <- data.frame("year" =  c(1961:1976),
                 "score" = tmp_similar[[1]])
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +
  geom_smooth(method = "lm", size = 1.5) + ggtitle("blues vs (black, blacks)")
p


df <- data.frame("year" =  c(1961:1976),
                 "score" = tmp_similar[[2]])
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +
  geom_smooth(method = "lm", size = 1.5) + ggtitle("blues vs (negro, negros, black, blacks)")
p

df <- data.frame("year" =  c(1961:1976),
                 "score" = tmp_similar[[3]])
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +
  geom_smooth(method = "lm", size = 1.5) + ggtitle("blues vs (negroes, negro)")
p

df <- data.frame("year" =  c(1961:1976),
                 "score" = tmp_similar[[4]])
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +
  geom_smooth(method = "lm", size = 1.5) + ggtitle("blues vs (tone, tones)")
p

df <- data.frame("year" =  c(1961:1976),
                 "score" = tmp_similar[[5]])
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +
  geom_smooth(method = "lm", size = 1.5) + ggtitle("blues vs (language)")
p

df <- data.frame("year" =  c(1961:1976),
                 "score" = tmp_similar[[6]])
library(ggplot2)
p <- qplot(year, score, data = df, xlab = "year", ylab = "association score") +
  geom_smooth(method = "lm", size = 1.5) + ggtitle("blues vs (language, tone (s), rhythm (s))")
p

sink("../output/Ebony_word_neighbors/blues.txt")
names(tmp_blues) <- paste0("Ebony_", 1961:1976)
print(tmp_blues)
sink()

sink("../output/Ebony_word_neighbors/black.txt")
names(tmp_black) <- paste0("Ebony_", 1961:1976)
print(tmp_black)
sink()

sink("../output/Ebony_word_neighbors/negro.txt")
names(tmp_negro) <- paste0("Ebony_", 1961:1976)
print(tmp_negro)
sink()

sink("../output/Ebony_word_neighbors/negro_black.txt")
names(tmp_negro_black) <- paste0("Ebony_", 1961:1976)
print(tmp_negro_black)
sink()

sink("../output/Ebony_word_neighbors/tone.txt")
names(tmp_tones) <- paste0("Ebony_", 1961:1976)
print(tmp_tones)
sink()

sink("../output//Ebony_word_neighbors/rhythm.txt")
names(tmp_rhythms) <- paste0("Ebony_", 1961:1976)
print(tmp_rhythms)
sink()

sink("../output/Ebony_word_neighbors/language.txt")
names(tmp_language) <- paste0("Ebony_", 1961:1976)
print(tmp_language)
sink()

