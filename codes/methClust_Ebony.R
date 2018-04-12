
########## methClust on Ebony  ################

dirs <- c("../Ebony/1961/", "../Ebony/1962/", "../Ebony/1963/",
          "../Ebony/1964/", "../Ebony/1965/", "../Ebony/1966/",
          "../Ebony/1967/", "../Ebony/1968/", "../Ebony/1969/",
          "../Ebony/1970/", "../Ebony/1971/", "../Ebony/1972/",
          "../Ebony/1973/", "../Ebony/1974/", "../Ebony/1975/",
          "../Ebony/1976/")

library(plyr)
df <- list()

for(d in 1:length(dirs)){
  ll <- list.files(paste0(dirs[d]), pattern = "19")
  
  Sys.setlocale('LC_ALL','C')
  
  all_names <- c()
  for(m in 1:length(ll)){
    sentences<-scan(paste0(dirs[d], ll[m]),"character",sep="\n");
    #Replace full stop and comma
    sentences<-gsub("\\.","",sentences)
    sentences<-gsub("\\,","",sentences)
    #Split sentence
    words<-strsplit(sentences," ")
    #Calculate word frequencies
    words.freq<-table(unlist(words));
    all_names <- union(all_names , names(words.freq))
  }
  
  tab <- matrix(0, length(ll), length(all_names))
  colnames(tab) <- all_names
  
  for(m in 1:length(ll)){
    sentences<-scan(paste0(dirs[d], ll[m]),"character",sep="\n");
    #Replace full stop and comma
    sentences<-gsub("\\.","",sentences)
    sentences<-gsub("\\,","",sentences)
    #Split sentence
    words<-strsplit(sentences," ")
    #Calculate word frequencies
    words.freq<-table(unlist(words));
    tab[m, match(names(words.freq), colnames(tab))] <- as.numeric(words.freq)
  }
  
  tab_pa <- tab
  tab_pa[tab_pa > 0] = 1
  
  df[[d]] <- colSums(tab_pa)
}

words <- c()
for(m in 1:length(df)){
  words <- union(words, names(df[[m]]))
}

mat <- matrix(0, length(df), length(words))
for(n in 1:length(df)){
  mat[n, match(names(df[[n]]),words)] <- df[[n]]
}

colnames(mat) <- words
rownames(mat) <- dirs

mat2 <- mat
mat3 <- mat2[, -grep("tl", substring(colnames(mat2),1,2))]
meth <- mat3
unmeth <- 12 - meth

library(methClust)
out <- meth_topics(meth, unmeth, K=2, tol =10)

save(out, file = "../output/methClust_Ebony_2.rda")
colnames(meth)[order(out$freq[,2] - out$freq[,1], decreasing = TRUE)[1:60]]

colnames(meth)[order(out$freq[,1] - out$freq[,2], decreasing = TRUE)[1:60]]

omega <- out$omega
annotation <- data.frame(
  sample_id = paste0("X", c(1:NROW(omega))),
  tissue_label = factor(1961:1976,
                        levels = 1961:1976))

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


