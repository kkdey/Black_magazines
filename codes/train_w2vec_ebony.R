
library(wordVectors)
dirs <- c("../Ebony/1961/", "../Ebony/1962/", "../Ebony/1963/",
          "../Ebony/1964/", "../Ebony/1965/", "../Ebony/1966/",
          "../Ebony/1967/", "../Ebony/1968/", "../Ebony/1969/",
          "../Ebony/1970/", "../Ebony/1971/", "../Ebony/1972/",
          "../Ebony/1973/", "../Ebony/1974/", "../Ebony/1975/",
          "../Ebony/1976/")

## civil rights bigram
## black power
## black panther
## martin luther king
## malcolm x
## dr king


for(m in 1:length(dirs)){
    model = train_word2vec(paste0(dirs[m], "pooled.txt"),
                           paste0(dirs[m], "pooled_word2vec.bin"),
                           vectors=100,threads=10,window=10,
                           iter=30, min_count = 3, negative_samples=0, 
                           force = TRUE)
}
