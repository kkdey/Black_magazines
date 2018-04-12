

##############  Lauren chosen words Ebony   #####################

tab_ebony <- get(load("../output/table_word_frequencies_ebony.rda"))
all_words_ebony <- get(load("../output/all_words_ebony.rda"))

tab_ebony_normalized <- t(apply(tab_ebony, 1, function(x) return(x/sum(x))))
rownames(tab_ebony_normalized) <- paste0("Ebony_", 1961:1976)

plot(tab_ebony_normalized[, "rights"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "rights")
axis(1, at=1:dim(tab_ebony_normalized)[1], 
     rownames(tab_ebony_normalized), las = 2, cex.axis = 0.7)

plot(tab_ebony_normalized[, "vietnam"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "vietnam")
axis(1, at=1:dim(tab_ebony_normalized)[1], 
     rownames(tab_ebony_normalized), las = 2, cex.axis = 0.7)

plot(tab_ebony_normalized[, "nonviolence"] + tab_ebony_normalized[, "nonviolent"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "nonviolence")
axis(1, at=1:dim(tab_ebony_normalized)[1], 
     rownames(tab_ebony_normalized), las = 2, cex.axis = 0.7)

plot(tab_ebony_normalized[, "violence"] + tab_ebony_normalized[, "violent"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "violence")
axis(1, at=1:dim(tab_ebony_normalized)[1], 
     rownames(tab_ebony_normalized), las = 2, cex.axis = 0.7)

plot(tab_ebony_normalized[, "beatles"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "vietnam")
axis(1, at=1:dim(tab_ebony_normalized)[1], 
     rownames(tab_ebony_normalized), las = 2, cex.axis = 0.7)


plot(tab_ebony_normalized[, "drugs"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "vietnam")
axis(1, at=1:dim(tab_ebony_normalized)[1], 
     rownames(tab_ebony_normalized), las = 2, cex.axis = 0.7)

plot(tab_ebony_normalized[, "hippie"] + tab_ebony_normalized[, "hippies"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "hippie")
axis(1, at=1:dim(tab_ebony_normalized)[1], 
     rownames(tab_ebony_normalized), las = 2, cex.axis = 0.7)



plot(tab_ebony_normalized[, "politics"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "politics")
axis(1, at=1:dim(tab_ebony_normalized)[1], 
     rownames(tab_ebony_normalized), las = 2, cex.axis = 0.7)

plot(tab_ebony_normalized[, "time"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "time")
axis(1, at=1:dim(tab_ebony_normalized)[1], 
     rownames(tab_ebony_normalized), las = 2, cex.axis = 0.7)


plot(tab_ebony_normalized[, "sick"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "sick")
axis(1, at=1:dim(tab_ebony_normalized)[1], 
     rownames(tab_ebony_normalized), las = 2, cex.axis = 0.7)

plot(tab_ebony_normalized[, "ill"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "ill")
axis(1, at=1:dim(tab_ebony_normalized)[1], 
     rownames(tab_ebony_normalized), las = 2, cex.axis = 0.7)

plot(tab_ebony_normalized[, "illness"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "illness")
axis(1, at=1:dim(tab_ebony_normalized)[1], 
     rownames(tab_ebony_normalized), las = 2, cex.axis = 0.7)

plot(tab_ebony_normalized[, "culture"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "culture")
axis(1, at=1:dim(tab_ebony_normalized)[1], 
     rownames(tab_ebony_normalized), las = 2, cex.axis = 0.7)

plot(tab_ebony_normalized[, "afroamerican"] + tab_ebony_normalized[, "africanamerican"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "afroamerican")
axis(1, at=1:dim(tab_ebony_normalized)[1], 
     rownames(tab_ebony_normalized), las = 2, cex.axis = 0.7)

plot(tab_ebony_normalized[, "africanamerican"], 
     col = "red", pch = 20, cex = 1.5, xaxt = "n", 
     ylab = "occurrence prop.", xlab = "", main = "africanamerican")
axis(1, at=1:dim(tab_ebony_normalized)[1], 
     rownames(tab_ebony_normalized), las = 2, cex.axis = 0.7)


tmp_rights <- list()
tmp_politics <- list()
tmp_time <- list()
tmp_sick <- list()
tmp_ill <- list()
tmp_illness <- list()
tmp_culture <- list()
tmp_afroamerican <- list()

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

sink("../output/Ebony_list_words/rights.txt")
names(tmp_rights) <- paste0("Ebony_", 1961:1976)
print(tmp_rights)
sink()

sink("../output/Ebony_list_words/politics.txt")
names(tmp_politics) <- paste0("Ebony_", 1961:1976)
print(tmp_politics)
sink()

sink("../output/Ebony_list_words/time.txt")
names(tmp_time) <- paste0("Ebony_", 1961:1976)
print(tmp_time)
sink()

sink("../output/Ebony_list_words/sick.txt")
names(tmp_sick) <- paste0("Ebony_", 1961:1976)
print(tmp_sick)
sink()

sink("../output/Ebony_list_words/ill.txt")
names(tmp_ill) <- paste0("Ebony_", 1961:1976)
print(tmp_ill)
sink()

sink("../output/Ebony_list_words/illness.txt")
names(tmp_illness) <- paste0("Ebony_", 1961:1976)
print(tmp_illness)
sink()

sink("../output/Ebony_list_words/culture.txt")
names(tmp_culture) <- paste0("Ebony_", 1961:1976)
print(tmp_culture)
sink()

sink("../output/Ebony_list_words/afroamerican.txt")
names(tmp_afroamerican) <- paste0("Ebony_", 1961:1976)
print(tmp_afroamerican)
sink()



