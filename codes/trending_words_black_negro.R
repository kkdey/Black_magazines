

df <- get(load(file = "../output/black_words_assoc.rda"))

z1 <- df[,1]/df[,2]
rownames(df)[order(z1, decreasing = FALSE)[1:100]]
z2 <- df[,3]/df[,4]
rownames(df)[order(z2, decreasing = FALSE)[1:100]]


df <- get(load(file = "../output/blacks_words_assoc.rda"))

z1 <- df[,1]/df[,2]
rownames(df)[order(z1, decreasing = FALSE)[1:100]]
z2 <- df[,3]/df[,4]
rownames(df)[order(z2, decreasing = FALSE)[1:100]]


df1 <- get(load(file = "../output/blacks_words_assoc.rda"))
z1 <- df1[,1]/df1[,2]
names1 <- rownames(df1)[order(z1, decreasing = FALSE)[1:300]]

df2 <- get(load(file = "../output/black_words_assoc.rda"))
z2 <- df2[,1]/df2[,2]
names2 <- rownames(df2)[order(z2, decreasing = FALSE)[1:300]]

setdiff(names1, names2)


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


