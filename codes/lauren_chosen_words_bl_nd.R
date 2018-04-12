

############  Lauren chosen words Negro Digest/Black World  ####################

tab_bl_nd <- get(load("../output/table_word_frequencies.rda"))
all_words_bl_nd <- get(load("../output/all_words.rda"))

tab_bl_nd_normalized <- t(apply(tab_bl_nd, 1, function(x) return(x/sum(x))))
rownames(tab_bl_nd_normalized) <- c(paste0("ND_", 1961:1970), paste0("BL_", 1970:1976))

plot(tab_bl_nd_normalized[, "rights"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "rights")
axis(1, at=1:dim(tab_bl_nd_normalized)[1], 
     rownames(tab_bl_nd_normalized), las = 2, cex.axis = 0.7)

plot(tab_bl_nd_normalized[, "vietnam"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "vietnam")
axis(1, at=1:dim(tab_bl_nd_normalized)[1], 
     rownames(tab_bl_nd_normalized), las = 2, cex.axis = 0.7)

plot(tab_bl_nd_normalized[, "drugs"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "vietnam")
axis(1, at=1:dim(tab_bl_nd_normalized)[1], 
     rownames(tab_bl_nd_normalized), las = 2, cex.axis = 0.7)

plot(tab_bl_nd_normalized[, "hendricks"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "vietnam")
axis(1, at=1:dim(tab_bl_nd_normalized)[1], 
     rownames(tab_bl_nd_normalized), las = 2, cex.axis = 0.7)


plot(tab_bl_nd_normalized[, "hippie"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "vietnam")
axis(1, at=1:dim(tab_bl_nd_normalized)[1], 
     rownames(tab_bl_nd_normalized), las = 2, cex.axis = 0.7)

plot(tab_bl_nd_normalized[, "politics"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "politics")
axis(1, at=1:dim(tab_bl_nd_normalized)[1], 
     rownames(tab_bl_nd_normalized), las = 2, cex.axis = 0.7)

plot(tab_bl_nd_normalized[, "time"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "time")
axis(1, at=1:dim(tab_bl_nd_normalized)[1], 
     rownames(tab_bl_nd_normalized), las = 2, cex.axis = 0.7)


plot(tab_bl_nd_normalized[, "sick"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "sick")
axis(1, at=1:dim(tab_bl_nd_normalized)[1], 
     rownames(tab_bl_nd_normalized), las = 2, cex.axis = 0.7)

plot(tab_bl_nd_normalized[, "ill"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "ill")
axis(1, at=1:dim(tab_bl_nd_normalized)[1], 
     rownames(tab_bl_nd_normalized), las = 2, cex.axis = 0.7)

plot(tab_bl_nd_normalized[, "illness"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "illness")
axis(1, at=1:dim(tab_bl_nd_normalized)[1], 
     rownames(tab_bl_nd_normalized), las = 2, cex.axis = 0.7)

plot(tab_bl_nd_normalized[, "culture"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "culture")
axis(1, at=1:dim(tab_bl_nd_normalized)[1], 
     rownames(tab_bl_nd_normalized), las = 2, cex.axis = 0.7)

plot(tab_bl_nd_normalized[, "afroamerican"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "culture")
axis(1, at=1:dim(tab_bl_nd_normalized)[1], 
     rownames(tab_bl_nd_normalized), las = 2, cex.axis = 0.7)

plot(tab_bl_nd_normalized[, "huey"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "culture")
axis(1, at=1:dim(tab_bl_nd_normalized)[1], 
     rownames(tab_bl_nd_normalized), las = 2, cex.axis = 0.7)

plot(tab_bl_nd_normalized[, "hoover"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "culture")
axis(1, at=1:dim(tab_bl_nd_normalized)[1], 
     rownames(tab_bl_nd_normalized), las = 2, cex.axis = 0.7)



plot(tab_bl_nd_normalized[, "africanamerican"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "culture")
axis(1, at=1:dim(tab_bl_nd_normalized)[1], 
     rownames(tab_bl_nd_normalized), las = 2, cex.axis = 0.7)



tmp_rights <- list()
tmp_politics <- list()
tmp_time <- list()
tmp_sick <- list()
tmp_ill <- list()
tmp_illness <- list()
tmp_culture <- list()
tmp_afroamerican <- list()

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

model_list <- list()
for(m in 1:length(dirs)){
  model_list[[m]] = read.vectors(paste0(dirs[m], "pooled_word2vec.bin"))
}
for(m in 1:length(dirs)){
  tmp_rights[[m]] <- model_list[[m]] %>% closest_to(c("rights"), n = 30)
  tmp_politics[[m]] <- model_list[[m]] %>% closest_to(c("politics"), n = 30)
  tmp_time[[m]] <- model_list[[m]] %>% closest_to(c("time"), n = 30)
  tmp_sick[[m]] <- model_list[[m]] %>% closest_to(c("sick"), n = 30)
  tmp_ill[[m]] <- model_list[[m]] %>% closest_to(c("ill"), n = 30)
  tmp_illness[[m]] <- model_list[[m]] %>% closest_to(c("illness"), n = 30)
  tmp_culture[[m]] <- model_list[[m]] %>% closest_to(c("culture"), n = 30)
  tmp_afroamerican[[m]] <- model_list[[m]] %>% closest_to(c("afroamerican", "africanamerican"), n = 30)
}

sink("../output/BL_ND_list_words/rights.txt")
names(tmp_rights) <- c(paste0("ND_", 1961:1970), paste0("BL_", 1970:1976))
print(tmp_rights)
sink()

sink("../output/BL_ND_list_words/politics.txt")
names(tmp_politics) <- c(paste0("ND_", 1961:1970), paste0("BL_", 1970:1976))
print(tmp_politics)
sink()

sink("../output/BL_ND_list_words/time.txt")
names(tmp_time) <- c(paste0("ND_", 1961:1970), paste0("BL_", 1970:1976))
print(tmp_time)
sink()

sink("../output/BL_ND_list_words/sick.txt")
names(tmp_sick) <- c(paste0("ND_", 1961:1970), paste0("BL_", 1970:1976))
print(tmp_sick)
sink()

sink("../output/BL_ND_list_words/ill.txt")
names(tmp_ill) <- c(paste0("ND_", 1961:1970), paste0("BL_", 1970:1976))
print(tmp_ill)
sink()

sink("../output/BL_ND_list_words/illness.txt")
names(tmp_illness) <- c(paste0("ND_", 1961:1970), paste0("BL_", 1970:1976))
print(tmp_illness)
sink()

sink("../output/BL_ND_list_words/culture.txt")
names(tmp_culture) <- c(paste0("ND_", 1961:1970), paste0("BL_", 1970:1976))
print(tmp_culture)
sink()

sink("../output/BL_ND_list_words/afroamerican.txt")
names(tmp_afroamerican) <- c(paste0("ND_", 1961:1970), paste0("BL_", 1970:1976))
print(tmp_afroamerican)
sink()



