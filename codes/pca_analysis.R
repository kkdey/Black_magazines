
#########  Principal Components Analysis and t-SNE  ###################


tab <- get(load("../output/table_word_frequencies.rda"))
one_occur_words <- apply(tab, 2, function(x) return(sum(x[x!=0])))
tab2 <- tab[, which(one_occur_words > 2)]

pca <- prcomp(tab2)
pca_projects <- pca$x[,1:2]

colors <- factor(c(rep(paste0("ND"), 10), rep(paste0("BL"), 7)))
plot(pca$x[,1], pca$x[,2], col = colors)