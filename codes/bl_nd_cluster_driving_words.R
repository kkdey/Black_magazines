

############  Black World Negro Digest  driving words   #################

tab_bl_nd <- get(load("../output/table_word_frequencies.rda"))
all_words_bl_nd <- get(load("../output/all_words.rda"))

tab_bl_nd_normalized <- t(apply(tab_bl_nd, 1, function(x) return(x/sum(x))))
rownames(tab_bl_nd_normalized) <- c(paste0("ND_", 1961:1970), paste0("BL_", 1970:1976))

plot(tab_bl_nd_normalized[, "literature"] + tab_bl_nd_normalized[, "literary"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "literature")
axis(1, at=1:dim(tab_bl_nd_normalized)[1], 
     rownames(tab_bl_nd_normalized), las = 2, cex.axis = 0.7)

plot(tab_bl_nd_normalized[, "poet"] + tab_bl_nd_normalized[, "poets"] + tab_bl_nd_normalized[, "poetry"] + 
       tab_bl_nd_normalized[, "poem"] + tab_bl_nd_normalized[, "poems"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "poetry")
axis(1, at=1:dim(tab_bl_nd_normalized)[1], 
     rownames(tab_bl_nd_normalized), las = 2, cex.axis = 0.7)

plot(tab_bl_nd_normalized[, "theater"] + tab_bl_nd_normalized[, "drama"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "drama")
axis(1, at=1:dim(tab_bl_nd_normalized)[1], 
     rownames(tab_bl_nd_normalized), las = 2, cex.axis = 0.7)


plot(tab_bl_nd_normalized[, "african"] + tab_bl_nd_normalized[, "afrikan"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "african")
axis(1, at=1:dim(tab_bl_nd_normalized)[1], 
     rownames(tab_bl_nd_normalized), las = 2, cex.axis = 0.7)

plot(tab_bl_nd_normalized[, "civil_rights"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "civil rights")
axis(1, at=1:dim(tab_bl_nd_normalized)[1], 
     rownames(tab_bl_nd_normalized), las = 2, cex.axis = 0.7)


plot(tab_bl_nd_normalized[, "righters"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "righters")
axis(1, at=1:dim(tab_bl_nd_normalized)[1], 
     rownames(tab_bl_nd_normalized), las = 2, cex.axis = 0.7)

plot(tab_bl_nd_normalized[, "discrimination"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "discrimination")
axis(1, at=1:dim(tab_bl_nd_normalized)[1], 
     rownames(tab_bl_nd_normalized), las = 2, cex.axis = 0.7)

plot(tab_bl_nd_normalized[, "nonviolent"] + tab_bl_nd_normalized[, "nonviolence"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "nonviolence")
axis(1, at=1:dim(tab_bl_nd_normalized)[1], 
     rownames(tab_bl_nd_normalized), las = 2, cex.axis = 0.7)


plot(tab_bl_nd_normalized[, "black_panther"] + tab_bl_nd_normalized[, "black_panthers"]
     + tab_bl_nd_normalized[, "panthers"] + tab_bl_nd_normalized[, "panther"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "black panther")
axis(1, at=1:dim(tab_bl_nd_normalized)[1], 
     rownames(tab_bl_nd_normalized), las = 2, cex.axis = 0.7)

plot(tab_bl_nd_normalized[, "race"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "race")
axis(1, at=1:dim(tab_bl_nd_normalized)[1], 
     rownames(tab_bl_nd_normalized), las = 2, cex.axis = 0.7)


