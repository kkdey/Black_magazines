

###############  Ebony vs Black World Negro Digest   ###################

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
     ylab = "proportion of occurrence", xlab = "", main = "Dodgers occurrence")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)

plot(tab_pooled_normalized[, "baseball"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "proportion of occurrence", xlab = "", main = "baseball occurrence")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)

plot(tab_pooled_normalized[, "basketball"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "proportion of occurrence", xlab = "", main = "basketball occurrence")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)

plot(tab_pooled_normalized[, "football"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "proportion of occurrence", xlab = "", main = "football occurrence")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)

plot(tab_pooled_normalized[, "chamberlain"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "proportion of occurrence", xlab = "", main = "chamberlain occurrence")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)

plot(tab_pooled_normalized[, "jerry"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "proportion of occurrence", xlab = "", main = "jerry occurrence")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)


plot(tab_pooled_normalized[, "pitching"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "proportion of occurrence", xlab = "", main = "pitching occurrence")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)

plot(tab_pooled_normalized[, "sox"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "proportion of occurrence", xlab = "", main = "sox occurrence")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)

plot(tab_pooled_normalized[, "astros"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "proportion of occurrence", xlab = "", main = "astros occurrence")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)

plot(tab_pooled_normalized[, "wilt"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "proportion of occurrence", xlab = "", main = "wilt occurrence")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)


################   Topics covered by Negro Digest  ####################

plot(tab_pooled_normalized[, "theater"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "proportion of occurrence", xlab = "", main = "theater occurrence")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)

plot(tab_pooled_normalized[, "poetry"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "proportion of occurrence", xlab = "", main = "poetry occurrence")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)

plot(tab_pooled_normalized[, "poet"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "proportion of occurrence", xlab = "", main = "poet occurrence")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)

plot(tab_pooled_normalized[, "blues"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "proportion of occurrence", xlab = "", main = "blues occurrence")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)

plot(tab_pooled_normalized[, "jazz"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "proportion of occurrence", xlab = "", main = "jazz occurrence")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)

plot(tab_pooled_normalized[, "music"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "proportion of occurrence", xlab = "", main = "music occurrence")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)

plot(tab_pooled_normalized[, "song"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "proportion of occurrence", xlab = "", main = "song occurrence")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)

plot(tab_pooled_normalized[, "literature"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "proportion of occurrence", xlab = "", main = "literature occurrence")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)

plot(tab_pooled_normalized[, "language"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "proportion of occurrence", xlab = "", main = "language occurrence")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)

plot(tab_pooled_normalized[, "arts"] + tab_pooled_normalized[, "art"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "proportion of occurrence", xlab = "", main = "arts occurrence")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)


plot(tab_pooled_normalized[, "africa"] + tab_pooled_normalized[, "african"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "proportion of occurrence", xlab = "", main = "africa occurrence")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)

plot(tab_pooled_normalized[, "shit"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "proportion of occurrence", xlab = "", main = "shit occurrence")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)

plot(tab_pooled_normalized[, "civil"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "proportion of occurrence", xlab = "", main = "civil occurrence")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)

plot(tab_pooled_normalized[, "rights"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "proportion of occurrence", xlab = "", main = "rights occurrence")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)

plot(tab_pooled_normalized[, "astronaut"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "proportion of occurrence", xlab = "", main = "astronaut occurrence")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)

plot(tab_pooled_normalized[, "dwight"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "proportion of occurrence", xlab = "", main = "dwight occurrence")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)

plot(tab_pooled_normalized[, "armstrong"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "proportion of occurrence", xlab = "", main = "armstrong occurrence")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)

plot(tab_pooled_normalized[, "panther"] + tab_pooled_normalized[, "panthers"], col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "proportion of occurrence", xlab = "", main = "panthers occurrence")
axis(1, at=1:dim(tab_pooled_normalized)[1], 
     rownames(tab_pooled_normalized), las = 2, cex.axis = 0.7)
