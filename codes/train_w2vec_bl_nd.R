

##############  Train word2vec models for Black World/Negro Digest

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
  model = train_word2vec(paste0(dirs[m], "pooled.txt"),
                         paste0(dirs[m], "pooled_word2vec.bin"),
                         vectors=100,threads=10,window=10,
                         iter=30, min_count = 3, negative_samples=0, 
                         force = TRUE)
}

