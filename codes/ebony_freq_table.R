dirs <- c("../Ebony/1961/", "../Ebony/1962/", "../Ebony/1963/",
          "../Ebony/1964/", "../Ebony/1965/", "../Ebony/1966/",
          "../Ebony/1967/", "../Ebony/1968/", "../Ebony/1969/",
          "../Ebony/1970/", "../Ebony/1971/", "../Ebony/1972/",
          "../Ebony/1973/", "../Ebony/1974/", "../Ebony/1975/",
          "../Ebony/1976/")

library(wordVectors)
for(m in 1:length(dirs)){
  out <- prep_word2vec(origin = dirs[m], destination = paste0(dirs[m],"pooled.txt"), lowercase = T)
}

word_count<-function(txt_doc){
  con<-file(txt_doc, "r", blocking=FALSE)
  x<-readLines(con)
  #Remove YAML front matter on Rmd
  if(length(grep("---",x))>0){x<-x[-seq(1,max(grep("---",x)))]}
  wrds<-0
  for(line in x){
    #Removes non character and splits
    split_line<-strsplit(gsub("[^[:alnum:] ]", "", line), " +")[[1]]
    #Removes empty string
    split_line<-split_line[split_line!=""]
    wrds<-wrds+length(split_line)
  }
  return(wrds)
}


tab <- c()
for(m in 1:length(dirs)){
  sentences<-scan(paste0(dirs[8], "Apr_1968.txt"),"character",sep="\n");
  #Replace full stop and comma
  sentences<-gsub("\\.","",sentences)
  sentences<-gsub("\\,","",sentences)
  #Split sentence
  words<-strsplit(sentences," ")
  #Calculate word frequencies
  words.freq<-table(unlist(words));
  
  usage_negro_1 <- words.freq[match("negro", names(words.freq))] 
  if(is.na(usage_negro_1)) usage_negro_1 <- 0
  usage_negro_2 <- words.freq[match("negros", names(words.freq))]
  if(is.na(usage_negro_2)) usage_negro_2 <- 0
  usage_negro_3 <- words.freq[match("negroes", names(words.freq))]
  if(is.na(usage_negro_3)) usage_negro_3 <- 0
  
  usage_negro <- usage_negro_1 + usage_negro_2 + usage_negro_3
  
  usage_black_1 <- words.freq[match("black", names(words.freq))] 
  if(is.na(usage_black_1)) usage_black_1 <- 0
  usage_black_2 <- words.freq[match("blacks", names(words.freq))] 
  if(is.na(usage_black_2)) usage_black_2 <- 0
  
  usage_black <- usage_black_1 + usage_black_2 
  
  prop_negro <- usage_negro/sum(words.freq)
  prop_black <- usage_black/sum(words.freq)
  
  tab <- rbind(tab, cbind.data.frame(prop_negro, prop_black))
}

rownames(tab) <- paste0("Ebony_", 1961:1976)

plot(tab[,1], type = "l", col= "red", ylim = c(0, 0.02))
lines(tab[,2], col= "blue")

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

save(all_names, file = "../output/all_words_ebony.rda")

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


save(tab, file = "../output/table_word_frequencies_ebony.rda")

