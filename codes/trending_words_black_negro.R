

df <- get(load(file = "../output/black_words_assoc.rda"))

z1 <- df[,1]/df[,2]
rownames(df)[order(z1, decreasing = FALSE)[1:100]]
z2 <- df[,3]/df[,4]
rownames(df)[order(z2, decreasing = FALSE)[1:100]]


df <- get(load(file = "../output/negro_words_assoc.rda"))

z1 <- df[,1]/df[,2]
rownames(df)[order(z1, decreasing = FALSE)[1:100]]
#z2 <- df[,3]/df[,4]
#rownames(df)[order(z2, decreasing = FALSE)[1:100]]

df <- get(load(file = "../output/negro_words_assoc_BL_ND.rda"))

z1 <- df[,1]/df[,2]
rownames(df)[order(z1, decreasing = FALSE)[1:100]]
#z2 <- df[,3]/df[,4]
#rownames(df)[order(z2, decreasing = FALSE)[1:100]]



df <- get(load(file = "../output/negro_words_assoc.rda"))

z1 <- df[,1]/df[,2]
rownames(df)[order(z1, decreasing = TRUE)[1:100]]

df <- get(load(file = "../output/negro_words_assoc_BL_ND.rda"))

z1 <- df[,1]/df[,2]
rownames(df)[order(z1, decreasing = TRUE)[1:100]]



df <- get(load(file = "../output/negros_words_assoc.rda"))

z1 <- df[,1]/df[,2]
rownames(df)[order(z1, decreasing = TRUE)[1:100]]

z1 <- df[,3]/df[,4]
rownames(df)[order(z1, decreasing = TRUE)[1:100]]



df <- get(load(file = "../output/black_words_assoc_BL_ND.rda"))

z1 <- df[,1]/df[,2]
rownames(df)[order(z1, decreasing = FALSE)[1:100]]
z1 <- df[,3]/df[,4]
rownames(df)[order(z1, decreasing = FALSE)[1:100]]





df <- get(load(file = "../output/negros_words_assoc.rda"))

z1 <- df[,1]/df[,2]
rownames(df)[order(z1, decreasing = FALSE)[1:100]]
z2 <- df[,3]/df[,4]
rownames(df)[order(z2, decreasing = FALSE)[1:100]]


