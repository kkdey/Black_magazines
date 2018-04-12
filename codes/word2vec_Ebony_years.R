

##########  word2vec analysis Ebony (across years)  ##################

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
tmp_rights <- list()
tmp_black <- list()
tmp_negro <- list()
tmp_movement <- list()



for(m in 1:length(dirs)){
  tmp_king[[m]] <- model_list[[m]] %>% closest_to(c("martin_luther_king", "martin_luther_kings",
                                                    "dr_king", "dr_kings"), n = 30)
  tmp_panther[[m]] <- model_list[[m]] %>% closest_to(c("black_panther", "black_panthers"), n = 30)
  tmp_power[[m]] <- model_list[[m]] %>% closest_to(c("black_power"), n = 30)
  tmp_rights[[m]] <- model_list[[m]] %>% closest_to(c("civil_rights"), n = 30)
  tmp_negro[[m]] <- model_list[[m]] %>% closest_to(c("negro", "negroes"), n = 30)
  tmp_black[[m]] <- model_list[[m]] %>% closest_to(c("black", "blacks"), n = 30)
  tmp_movement[[m]] <- model_list[[m]] %>% closest_to(c("movement"), n = 30)
}

library(wordcloud)

wordcloud(c(letters, LETTERS, 0:9), seq(1, 1000, len = 62))

pdf(file = "../critique/images/wordcloud.pdf", height = 12, width = 12)
par(mfrow = c(4,4))
for(m in 1:16){
  wordcloud(tmp_king[[m]]$word[1:10], tmp_king[[m]][1:10,2], scale = c(1.5, 0.6), rot.per = 0,
            colors="red", ordered.colors = TRUE)
}
dev.off()

pdf(file = "../critique/images/wordcloud_movement.pdf", height = 12, width = 12)
par(mfrow = c(4,4))
for(m in 1:16){
  wordcloud(tmp_movement[[m]]$word[1:10], 
            tmp_movement[[m]][1:10,2], scale = c(1.3, 0.8), rot.per = 0,
            colors="red", ordered.colors = TRUE)
}
dev.off()
         