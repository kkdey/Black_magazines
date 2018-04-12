
###########  CorShrink analysis on 1968 Ebony data  ######################

library(wordVectors)
prep_word2vec("../Ebony/1968/", "../output/1968/tmp.txt")
original_word2vec <- train_word2vec("../Ebony/1968/pooled.txt",
                                    "../output/1968/original_1968.bin",
                                    vectors=200,
                                    threads=4,
                                    window=10, iter=50, min_count = 3,
                                    negative_samples=0)