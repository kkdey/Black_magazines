

############  Driving words Ebony and Black World /Negro Digest   ####################


tab_ebony <- get(load("../output/table_word_frequencies_ebony.rda"))
tab_bl_nd <- get(load("../output/table_word_frequencies.rda"))

all_words_ebony <- get(load("../output/all_words_ebony.rda"))
all_words_bl_nd <- get(load("../output/all_words.rda"))

common_words <- intersect(all_words_ebony, all_words_bl_nd)

tab_pooled <- rbind(tab_ebony[,match(common_words, all_words_ebony)],
                    tab_bl_nd[,match(common_words, all_words_bl_nd)])

rownames(tab_pooled) <- c(paste0("Ebony_", 1961:1976), 
                          paste0("ND_", 1961:1970), paste0("BL_", 1970:1976))

tab_pooled_normalized <- t(apply(tab_pooled, 1, function(x) return(x/sum(x))))

plot(tab_pooled_normalized[, "dodgers"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "", xlab = "", main = "dodgers")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)


plot(tab_pooled_normalized[, "cubs"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "", xlab = "", main = "cubs")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)


plot(tab_pooled_normalized[, "baseball"] + tab_pooled_normalized[, "baseballs"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "", xlab = "", main = "baseball")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)

plot(tab_pooled_normalized[, "basketball"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "", xlab = "", main = "basketball")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)

plot(tab_pooled_normalized[, "football"] + tab_pooled_normalized[, "footballs"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "", xlab = "", main = "football")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)

plot(tab_pooled_normalized[, "coach"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "", xlab = "", main = "coach")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)

plot(tab_pooled_normalized[, "team"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "", xlab = "", main = "team")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)


plot(tab_pooled_normalized[, "literature"] + tab_pooled_normalized[, "literary"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "", xlab = "", main = "literature")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)


plot(tab_pooled_normalized[, "theater"] + tab_pooled_normalized[, "drama"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "", xlab = "", main = "drama")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)


plot(tab_pooled_normalized[, "poetry"] + tab_pooled_normalized[, "poet"]
     + tab_pooled_normalized[, "poets"] + tab_pooled_normalized[, "poem"]
     + tab_pooled_normalized[, "poems"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "", xlab = "", main = "poetry")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)


plot(tab_pooled_normalized[, "civil_rights"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "", xlab = "", main = "civil rights")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)

plot(tab_pooled_normalized[, "black_panther"] + tab_pooled_normalized[, "black_panthers"]
     + tab_pooled_normalized[, "panther"] + tab_pooled_normalized[, "panthers"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "", xlab = "", main = "black panther")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)


