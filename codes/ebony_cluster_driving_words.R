

#############   Ebony cluster driving words   ###################

topic_clus <- get(load("../output/CountClust_wo_negro_black_k_2_Ebony_cutoff_2.rda"))
out <- ExtractTopFeatures(topic_clus$theta, top_features = 100, method = "poisson", options = "min")
driving_words <- apply(out$indices, c(1,2), function(x) return(rownames(topic_clus$theta)[x]))


tab_ebony <- get(load("../output/table_word_frequencies_ebony.rda"))
all_words_ebony <- get(load("../output/all_words_ebony.rda"))

tab_ebony_normalized <- t(apply(tab_ebony, 1, function(x) return(x/sum(x))))
rownames(tab_ebony_normalized) <- paste0("Ebony_", 1961:1976)

plot(tab_ebony_normalized[, "black_panther"] + tab_ebony_normalized[, "black_panthers"] +
     tab_ebony_normalized[, "panthers"] + tab_ebony_normalized[, "panther"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "black panther")
axis(1, at=1:dim(tab_ebony_normalized)[1], 
     rownames(tab_ebony_normalized), las = 2, cex.axis = 0.7)

plot(tab_ebony_normalized[, "civil_rights"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "civil rights")
axis(1, at=1:dim(tab_ebony_normalized)[1], 
     rownames(tab_ebony_normalized), las = 2, cex.axis = 0.7)


plot(tab_ebony_normalized[, "righters"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "righters")
axis(1, at=1:dim(tab_ebony_normalized)[1], 
     rownames(tab_ebony_normalized), las = 2, cex.axis = 0.7)

plot(tab_ebony_normalized[, "race"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "race")
axis(1, at=1:dim(tab_ebony_normalized)[1], 
     rownames(tab_ebony_normalized), las = 2, cex.axis = 0.7)
