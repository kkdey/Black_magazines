
############   Ebony preparation  #####################

sentences<-scan("../Ebony/Apr_1961.txt","character",sep="\n");
#Replace full stop and comma
sentences<-gsub("\\.","",sentences)
sentences<-gsub("\\,","",sentences)
#Split sentence
words<-strsplit(sentences," ")
#Calculate word frequencies
words.freq<-table(unlist(words));


model = train_word2vec("../Ebony/Apr_1961.txt","../Ebony/Apr_1961.bin",
                       vectors=200,threads=4,window=12,iter=5,negative_samples=0)