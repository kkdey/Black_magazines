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

all_names <- c()
for(m in 1:length(dirs)){
  sentences<-scan(paste0(dirs[m], "pooled.txt"),"character",sep="\n");
  #Replace full stop and comma
  sentences<-gsub("\\.","",sentences)
  sentences<-gsub("\\,","",sentences)
  #Split sentence
  words<-strsplit(sentences," ")
  #Calculate word frequencies
  words.freq<-table(unlist(words));
  all_names <- union(all_names , names(words.freq))
}

save(all_names, file = "../output/all_words.rda")

tab <- matrix(0, length(dirs), length(all_names))
colnames(tab) <- all_names
rownames(tab) <- c(paste0("ND_", 1961:1970), paste0("BL_", 1970:1976))

for(m in 1:length(dirs)){
  sentences<-scan(paste0(dirs[m], "pooled.txt"),"character",sep="\n");
  #Replace full stop and comma
  sentences<-gsub("\\.","",sentences)
  sentences<-gsub("\\,","",sentences)
  #Split sentence
  words<-strsplit(sentences," ")
  #Calculate word frequencies
  words.freq<-table(unlist(words));
  tab[m, match(names(words.freq), colnames(tab))] <- as.numeric(words.freq)
}

save(tab, file = "../output/table_word_frequencies.rda")

one_occur_words <- apply(tab, 2, function(x) return(sum(x[x!=0])))
tab2 <- tab[, which(one_occur_words > 2)]
topic_clus <- maptpx::topics(tab2, K=2, tol = 1)

omega <- topic_clus$omega
annotation <- data.frame(
  sample_id = paste0("X", c(1:NROW(omega))),
  tissue_label = factor(rownames(omega),
                        levels = rownames(omega)))

rownames(omega) <- annotation$sample_id;

StructureGGplot(omega = omega,
                annotation = annotation,
                palette = RColorBrewer::brewer.pal(8, "Accent"),
                yaxis_label = "Years of Publication",
                order_sample = TRUE,
                axis_tick = list(axis_ticks_length = .1,
                                 axis_ticks_lwd_y = .1,
                                 axis_ticks_lwd_x = .1,
                                 axis_label_size = 7,
                                 axis_label_face = "bold"))


out <- ExtractTopFeatures(topic_clus$theta, top_features = 50, method = "poisson", options = "min")
driving_words <- apply(out$indices, c(1,2), function(x) return(colnames(tab2)[x]))

##############################    literature and arts enrichment   #############################

tab <- c()
for(m in 1:length(dirs)){
  sentences<-scan(paste0(dirs[m], "pooled.txt"),"character",sep="\n");
  #Replace full stop and comma
  sentences<-gsub("\\.","",sentences)
  sentences<-gsub("\\,","",sentences)
  #Split sentence
  words<-strsplit(sentences," ")
  #Calculate word frequencies
  words.freq<-table(unlist(words));
  
  usage_theater <- words.freq[match("theater", names(words.freq))] 
  if(is.na(usage_theater)) usage_theater <- 0
  
  usage_broadside <- words.freq[match("broadside", names(words.freq))] 
  if(is.na(usage_broadside)) usage_broadside <- 0
  
  usage_poetry <- words.freq[match("poetry", names(words.freq))] 
  if(is.na(usage_poetry)) usage_poetry <- 0
  
  usage_lit <- words.freq[match("literature", names(words.freq))] 
  if(is.na(usage_lit)) usage_lit <- 0
  
  usage_music <- words.freq[match("music", names(words.freq))] 
  if(is.na(usage_music)) usage_music <- 0
  
  usage_drama <- words.freq[match("drama", names(words.freq))] 
  if(is.na(usage_drama)) usage_drama <- 0
  
  prop_theater <- usage_theater/sum(words.freq)
  prop_broadside <- usage_broadside/sum(words.freq)
  prop_poetry <- usage_poetry/sum(words.freq)
  prop_lit <- usage_lit/sum(words.freq)
  prop_music <- usage_music/sum(words.freq)
  prop_drama <- usage_drama/sum(words.freq)
  

  tab <- rbind(tab, cbind.data.frame(prop_theater, prop_broadside, prop_poetry, 
                                     prop_lit, prop_music, prop_drama))
}

rownames(tab) <- c(paste0("ND_", 1961:1970), paste0("BL_", 1970:1976))
charts.data <- data.frame("word" = c(rep("theater", 17), rep("broadside", 17),
                                     rep("poetry", 17), rep("literature", 17),
                                     rep("music", 17), rep("drama", 17)),
                          "year" = factor(c(rownames(tab), rownames(tab), rownames(tab), 
                                            rownames(tab), rownames(tab), rownames(tab)),
                                          levels = rownames(tab)),
                          "prop" = c(tab[,1], tab[,2], tab[,3], tab[,4], tab[,5], tab[,6]))
p1 <- ggplot() + geom_line(aes(y = prop, x = year, colour = word, group=word),
                           data = charts.data, stat="identity") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) + xlab("") + ylab("relative occurrence")
p1


############################    civil, right and righters   #####################################


tab <- c()
for(m in 1:length(dirs)){
  sentences<-scan(paste0(dirs[m], "pooled.txt"),"character",sep="\n");
  #Replace full stop and comma
  sentences<-gsub("\\.","",sentences)
  sentences<-gsub("\\,","",sentences)
  #Split sentence
  words<-strsplit(sentences," ")
  #Calculate word frequencies
  words.freq<-table(unlist(words));
  
  usage_civil <- words.freq[match("civil", names(words.freq))] 
  if(is.na(usage_civil)) usage_civil <- 0
  
  usage_rights <- words.freq[match("rights", names(words.freq))] 
  if(is.na(usage_rights)) usage_rights <- 0
  
  usage_righters <- words.freq[match("righters", names(words.freq))] 
  if(is.na(usage_righters)) usage_righters <- 0
  
  usage_segregation <- words.freq[match("segregation", names(words.freq))] 
  if(is.na(usage_segregation)) usage_segregation <- 0
  
  usage_movement <- words.freq[match("movement", names(words.freq))] 
  if(is.na(usage_movement)) usage_movement <- 0
  
  usage_protests <- words.freq[match("protests", names(words.freq))] 
  if(is.na(usage_protests)) usage_protests <- 0
  
  prop_civil <- usage_civil/sum(words.freq)
  prop_rights <- usage_rights/sum(words.freq)
  prop_righters <- usage_righters/sum(words.freq)
  prop_segregation <- usage_segregation/sum(words.freq)
  prop_movement <- usage_movement/sum(words.freq)
  prop_protests <- usage_protests/sum(words.freq)
  
  tab <- rbind(tab, cbind.data.frame(prop_civil, prop_rights, prop_righters, 
                                     prop_segregation, prop_movement, prop_protests))
}


rownames(tab) <- c(paste0("ND_", 1961:1970), paste0("BL_", 1970:1976))
charts.data <- data.frame("word" = c(rep("civil", 17), rep("rights", 17),
                                     rep("righters", 17),
                                     rep("segregation", 17), rep("movement", 17)),
                          "year" = factor(c(rownames(tab), rownames(tab), rownames(tab), 
                                            rownames(tab), rownames(tab)),
                                          levels = rownames(tab)),
                          "prop" = c(tab[,1], tab[,2], tab[,3], tab[,4], tab[,5]))
p1 <- ggplot() + geom_line(aes(y = prop, x = year, colour = word, group=word),
                           data = charts.data, stat="identity") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) + xlab("") + ylab("relative occurrence")
p1


############################   african use    #####################################


tab <- c()
for(m in 1:length(dirs)){
  sentences<-scan(paste0(dirs[m], "pooled.txt"),"character",sep="\n");
  #Replace full stop and comma
  sentences<-gsub("\\.","",sentences)
  sentences<-gsub("\\,","",sentences)
  #Split sentence
  words<-strsplit(sentences," ")
  #Calculate word frequencies
  words.freq<-table(unlist(words));
  
  usage_africa <- words.freq[match("africa", names(words.freq))] 
  if(is.na(usage_africa)) usage_africa <- 0
  
  usage_africans <- words.freq[match("africans", names(words.freq))] 
  if(is.na(usage_africans)) usage_africans <- 0
  
  usage_african <- words.freq[match("african", names(words.freq))] 
  if(is.na(usage_african)) usage_african <- 0
  
  usage_panafrican <- words.freq[match("panafrican", names(words.freq))] 
  if(is.na(usage_panafrican)) usage_panafrican <- 0
  
  usage_afroamerican <- words.freq[match("afroamerican", names(words.freq))] 
  if(is.na(usage_afroamerican)) usage_afroamerican <- 0
  
  prop_africa <- usage_africa/sum(words.freq)
  prop_africans <- usage_africans/sum(words.freq)
  prop_african <- usage_african/sum(words.freq)
  prop_panafrican <- usage_panafrican/sum(words.freq)
  prop_afroamerican <- usage_afroamerican/sum(words.freq)

  tab <- rbind(tab, cbind.data.frame(prop_africa, prop_africans, prop_african, 
                                     prop_panafrican, prop_afroamerican))
}


rownames(tab) <- c(paste0("ND_", 1961:1970), paste0("BL_", 1970:1976))
charts.data <- data.frame("word" = c(rep("africa", 17), rep("africans", 17),
                                     rep("african", 17),
                                     rep("panafrican", 17), rep("afroamerican", 17)),
                          "year" = factor(c(rownames(tab), rownames(tab), rownames(tab), 
                                            rownames(tab), rownames(tab)),
                                          levels = rownames(tab)),
                          "prop" = c(tab[,1], tab[,2], tab[,3], tab[,4], tab[,5]))
p1 <- ggplot() + geom_line(aes(y = prop, x = year, colour = word, group=word),
                           data = charts.data, stat="identity") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) + xlab("") + ylab("relative occurrence")
p1

