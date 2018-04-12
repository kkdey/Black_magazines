
library(wordVectors)
out <- prep_word2vec(origin = "../Negro_Digest/ND_1961/", destination = "../Negro_Digest/ND_1961/pooled.txt", lowercase = T)
out <- prep_word2vec(origin = "../Negro_Digest/ND_1962/", destination = "../Negro_Digest/ND_1962/pooled.txt", lowercase = T)
out <- prep_word2vec(origin = "../Negro_Digest/ND_1963/", destination = "../Negro_Digest/ND_1963/pooled.txt", lowercase = T)
out <- prep_word2vec(origin = "../Negro_Digest/ND_1964/", destination = "../Negro_Digest/ND_1964/pooled.txt", lowercase = T)
out <- prep_word2vec(origin = "../Negro_Digest/ND_1965/", destination = "../Negro_Digest/ND_1965/pooled.txt", lowercase = T)
out <- prep_word2vec(origin = "../Negro_Digest/ND_1966/", destination = "../Negro_Digest/ND_1966/pooled.txt", lowercase = T)
out <- prep_word2vec(origin = "../Negro_Digest/ND_1967/", destination = "../Negro_Digest/ND_1967/pooled.txt", lowercase = T)
out <- prep_word2vec(origin = "../Negro_Digest/ND_1968/", destination = "../Negro_Digest/ND_1968/pooled.txt", lowercase = T)
out <- prep_word2vec(origin = "../Negro_Digest/ND_1969/", destination = "../Negro_Digest/ND_1969/pooled.txt", lowercase = T)
out <- prep_word2vec(origin = "../Negro_Digest/ND_1970/", destination = "../Negro_Digest/ND_1970/pooled.txt", lowercase = T)


out <- prep_word2vec(origin = "../Black_World/BL_1970/", destination = "../Black_World/BL_1970/pooled.txt", lowercase = T)
out <- prep_word2vec(origin = "../Black_World/BL_1971/", destination = "../Black_World/BL_1971/pooled.txt", lowercase = T)
out <- prep_word2vec(origin = "../Black_World/BL_1972/", destination = "../Black_World/BL_1972/pooled.txt", lowercase = T)
out <- prep_word2vec(origin = "../Black_World/BL_1973/", destination = "../Black_World/BL_1973/pooled.txt", lowercase = T)
out <- prep_word2vec(origin = "../Black_World/BL_1974/", destination = "../Black_World/BL_1974/pooled.txt", lowercase = T)
out <- prep_word2vec(origin = "../Black_World/BL_1975/", destination = "../Black_World/BL_1975/pooled.txt", lowercase = T)
out <- prep_word2vec(origin = "../Black_World/BL_1976/", destination = "../Black_World/BL_1976/pooled.txt", lowercase = T)

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

rownames(tab) <- c(paste0("ND_", 1961:1970), paste0("BL_", 1970:1976))
save(tab, file = "../output/usage_negro_black_time.rda")

plot(tab[,1], type = "l", col= "red", ylim = c(0, 0.02))
lines(tab[,2], col= "blue")


charts.data <- data.frame("word" = c(rep("negro", 17), rep("black", 17)),
                          "year" = factor(c(rownames(tab), rownames(tab)),
                                          levels = rownames(tab)),
                          "prop" = c(tab[,1], tab[,2]))


p1 <- ggplot() + geom_line(aes(y = prop, x = year, colour = word, group=word),
                           data = charts.data, stat="identity") +
        theme(axis.text.x=element_text(angle=90, hjust=1)) + xlab("") + ylab("relative occurrence")
p1

