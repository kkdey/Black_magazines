
library(wordVectors)
dirs <- c("../BL_ND/BL_ND_1961/",
          "../BL_ND/BL_ND_1962/",
          "../BL_ND/BL_ND_1963/",
          "../BL_ND/BL_ND_1964/",
          "../BL_ND/BL_ND_1965/",
          "../BL_ND/BL_ND_1966/",
          "../BL_ND/BL_ND_1967/",
          "../BL_ND/BL_ND_1968/",
          "../BL_ND/BL_ND_1969/",
          "../BL_ND/BL_ND_1970/",
          "../BL_ND/BL_ND_1970/",
          "../BL_ND/BL_ND_1971/",
          "../BL_ND/BL_ND_1972/",
          "../BL_ND/BL_ND_1973/",
          "../BL_ND/BL_ND_1974/",
          "../BL_ND/BL_ND_1975/",
          "../BL_ND/BL_ND_1976/")


library(wordVectors)
for(m in 1:length(dirs)){
  out <- prep_word2vec(origin = dirs[m], destination = paste0(dirs[m], "pooled.txt"), lowercase = T)
  model = train_word2vec(paste0(dirs[m], "pooled.txt"),
                         paste0(dirs[m], "pooled_word2vec.bin"),
                         vectors=100,threads=10,window=10,
                         iter=50, min_count = 3, negative_samples=0,
                         force = TRUE)
}

