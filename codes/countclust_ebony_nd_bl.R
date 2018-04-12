
###########   CountClust on Ebony + Black World + Negro Digest  ##################

tab_ebony <- get(load("../output/table_word_frequencies_ebony.rda"))
tab_bl_nd <- get(load("../output/table_word_frequencies.rda"))

all_words_ebony <- get(load("../output/all_words_ebony.rda"))
all_words_bl_nd <- get(load("../output/all_words.rda"))

common_words <- intersect(all_words_ebony, all_words_bl_nd)

tab_pooled <- rbind(tab_ebony[,match(common_words, all_words_ebony)],
                    tab_bl_nd[,match(common_words, all_words_bl_nd)])

rownames(tab_pooled) <- c(paste0("Ebony_", 1961:1976), 
                          paste0("ND_", 1961:1970), paste0("BL_", 1970:1976))

one_occur_words <- apply(tab_pooled, 2, function(x) return(sum(x[x!=0])))
tab2_pooled <- tab_pooled[, which(one_occur_words > 2)]
tab2_pooled <- tab2_pooled[, which(nchar(colnames(tab2_pooled)) > 2)]

topic_clus <- maptpx::topics(tab2_pooled, K=2, tol = 1)
save(topic_clus, file = "../output/CountClust_k_2_Ebony_BL_ND_cutoff_2.rda")

topic_clus <- maptpx::topics(tab2_pooled, K=3, tol = 1)
save(topic_clus, file = "../output/CountClust_k_3_Ebony_BL_ND_cutoff_2.rda")

topic_clus <- maptpx::topics(tab2_pooled, K=4, tol = 1)
save(topic_clus, file = "../output/CountClust_k_4_Ebony_BL_ND_cutoff_2.rda")


##########################    K = 2   #####################################

library(CountClust)
topic_clus <- get(load("../output/CountClust_k_2_Ebony_BL_ND_cutoff_2.rda"))

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
driving_words <- apply(out$indices, c(1,2), function(x) return(rownames(topic_clus$theta)[x]))

######################   only Ebony articles   #####################################

tab_pooled_1 <- tab_pooled[1:16,]
one_occur_words_1 <- apply(tab_pooled_1, 2, function(x) return(sum(x[x!=0])))
tab2_pooled_1 <- tab_pooled_1[, which(one_occur_words_1 > 2)]
tab2_pooled_1 <- tab2_pooled_1[, which(nchar(colnames(tab2_pooled_1)) > 2)]
tab2_pooled_1 <- tab2_pooled_1[, which(colnames(tab2_pooled_1) != "tlie")]

topic_clus <- maptpx::topics(tab2_pooled_1, K=2, tol = 1)
save(topic_clus, file = "../output/CountClust_k_2_Ebony_cutoff_2.rda")

topic_clus <- maptpx::topics(tab2_pooled_1, K=3, tol = 1)
save(topic_clus, file = "../output/CountClust_k_3_Ebony_cutoff_2.rda")

topic_clus <- maptpx::topics(tab2_pooled_1, K=4, tol = 1)
save(topic_clus, file = "../output/CountClust_k_4_Ebony_cutoff_2.rda")

####################   K = 2 Ebony   ############################

topic_clus <- get(load("../output/CountClust_k_2_Ebony_cutoff_2.rda"))

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
driving_words <- apply(out$indices, c(1,2), function(x) return(rownames(topic_clus$theta)[x]))

#################   Remove black, blacks, negro, negroes   ##############

negro_words <- grep("negro", colnames(tab_pooled))
black_words <- match(c("black", "blacks", "blackman"), colnames(tab_pooled))

tab_pooled_filtered <- tab_pooled[, - union(negro_words, black_words)]


one_occur_words_2 <- apply(tab_pooled_filtered, 2, function(x) return(sum(x[x!=0])))
tab2_pooled_2 <- tab_pooled_filtered[, which(one_occur_words_2 > 2)]
tab2_pooled_2 <- tab2_pooled_2[, which(nchar(colnames(tab2_pooled_2)) > 2)]

topic_clus <- maptpx::topics(tab2_pooled_2, K=2, tol = 1)
save(topic_clus, file = "../output/CountClust_wo_negro_black_k_2_Ebony_BL_ND_cutoff_2.rda")

topic_clus <- maptpx::topics(tab2_pooled_2, K=3, tol = 1)
save(topic_clus, file = "../output/CountClust_wo_negro_black_k_3_Ebony_BL_ND_cutoff_2.rda")

topic_clus <- maptpx::topics(tab2_pooled_2, K=4, tol = 1)
save(topic_clus, file = "../output/CountClust_wo_negro_black_k_4_Ebony_BL_ND_cutoff_2.rda")

####################   K = 2 filtered   ############################

topic_clus <- get(load("../output/CountClust_wo_negro_black_k_4_Ebony_BL_ND_cutoff_2.rda"))

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
driving_words <- apply(out$indices, c(1,2), function(x) return(rownames(topic_clus$theta)[x]))

##############  only Ebony with filtered black and negro  ####################


tab_pooled_3 <- tab_pooled_filtered[1:16, which(nchar(colnames(tab_pooled_filtered)) > 2)]
one_occur_words_3 <- apply(tab_pooled_3, 2, function(x) return(sum(x[x!=0])))
tab2_pooled_3 <- tab_pooled_3[, which(one_occur_words_3 > 2)]
tab2_pooled_3 <- tab2_pooled_3[, which(colnames(tab2_pooled_3) != "tlie")]


topic_clus <- maptpx::topics(tab2_pooled_3, K=2, tol = 1)
save(topic_clus, file = "../output/CountClust_wo_negro_black_k_2_Ebony_cutoff_2.rda")

topic_clus <- maptpx::topics(tab2_pooled_3, K=3, tol = 1)
save(topic_clus, file = "../output/CountClust_wo_negro_black_k_3_Ebony_cutoff_2.rda")

topic_clus <- maptpx::topics(tab2_pooled_3, K=4, tol = 1)
save(topic_clus, file = "../output/CountClust_wo_negro_black_k_4_Ebony_cutoff_2.rda")


####################   K = 2 filtered Ebony   ############################

topic_clus <- get(load("../output/CountClust_wo_negro_black_k_2_Ebony_cutoff_2.rda"))

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
driving_words <- apply(out$indices, c(1,2), function(x) return(rownames(topic_clus$theta)[x]))


