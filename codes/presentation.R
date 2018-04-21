

##################   presentation   ##########################

##########   Trend of black and negro words   (Ebony)  #########################

tab_ebony <- get(load("../output/table_word_frequencies_ebony.rda"))
tab_ebony_normalized <- t(apply(tab_ebony, 1, function(x) return(x/sum(x))))
rownames(tab_ebony_normalized) <- paste0("Ebony_", 1961:1976)

black_prop <- tab_ebony_normalized[,c("black")] + tab_ebony_normalized[,c("blacks")]
negro_prop <- tab_ebony_normalized[,c("negro")] + tab_ebony_normalized[,c("negros")] + tab_ebony_normalized[,c("negroes")]

charts.data <- data.frame("word" = c(rep("negro", 16), rep("black", 16)),
                          "year" = factor(c(rownames(tab_ebony_normalized), rownames(tab_ebony_normalized)),
                                          levels = rownames(tab_ebony_normalized)),
                          "prop" = c(black_prop, negro_prop))


p1 <- ggplot() + geom_line(aes(y = prop, x = year, colour = word, group=word),
                           data = charts.data, stat="identity") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) + xlab("") + ylab("relative occurrence") +
  ggtitle("Ebony trends : black and negro words")
p1


##########   Trend of black and negro words   (BL/ND)  #########################

tab_bl_nd <- get(load("../output/table_word_frequencies.rda"))
tab_bl_nd_normalized <- t(apply(tab_bl_nd, 1, function(x) return(x/sum(x))))
rownames(tab_bl_nd_normalized) <-  c(paste0("ND_", 1961:1970), paste0("BL_", 1970:1976))

black_prop <- tab_bl_nd_normalized[,c("black")] + tab_bl_nd_normalized[,c("blacks")]
negro_prop <- tab_bl_nd_normalized[,c("negro")] + tab_bl_nd_normalized[,c("negros")] + tab_bl_nd_normalized[,c("negroes")]

charts.data <- data.frame("word" = c(rep("negro", 17), rep("black", 17)),
                          "year" = factor(c(rownames(tab_bl_nd_normalized), rownames(tab_bl_nd_normalized)),
                                          levels = rownames(tab_bl_nd_normalized)),
                          "prop" = c(black_prop, negro_prop))


p1 <- ggplot() + geom_line(aes(y = prop, x = year, colour = word, group=word),
                           data = charts.data, stat="identity") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) + xlab("") + ylab("relative occurrence") +
  ggtitle("BL/ND trends : black and negro words")
p1


####################   CountClust analysis (separate)  ###########################

topic_clus <- get(load("../output/CountClust_k_2_BL_ND_cutoff_2.rda"))

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


topic_clus_1 <- get(load("../output/CountClust_k_2_Ebony_cutoff_2.rda"))

omega <- topic_clus_1$omega
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



out <- ExtractTopFeatures(topic_clus_1$theta, top_features = 50, method = "poisson", options = "min")
driving_words <- apply(out$indices, c(1,2), function(x) return(rownames(topic_clus$theta)[x]))


##################   No black and no negro words   ##################################

topic_clus <- get(load("../output/CountClust_wo_negro_black_k_2_Ebony_cutoff_2.rda"))

omega <- topic_clus$omega
annotation <- data.frame(
  sample_id = paste0("X", c(1:NROW(omega))),
  tissue_label = factor(rownames(omega),
                        levels = rownames(omega)))

rownames(omega) <- annotation$sample_id;

StructureGGplot(omega = omega,
                annotation = annotation,
                palette = RColorBrewer::brewer.pal(8, "Accent")[1:2],
                yaxis_label = "Years of Publication",
                order_sample = TRUE,
                axis_tick = list(axis_ticks_length = .1,
                                 axis_ticks_lwd_y = .1,
                                 axis_ticks_lwd_x = .1,
                                 axis_label_size = 7,
                                 axis_label_face = "bold"))

out <- ExtractTopFeatures(topic_clus$theta, top_features = 50, method = "poisson", options = "min")
driving_words <- apply(out$indices, c(1,2), function(x) return(rownames(topic_clus$theta)[x]))

# tab_bl_nd <- get(load("../output/table_word_frequencies.rda"))
# negro_words <- grep("negro", colnames(tab_bl_nd))
# black_words <- match(c("black", "blacks"), colnames(tab_bl_nd))
#
# tab_bl_nd_filtered <- tab_bl_nd[, - union(negro_words, black_words)]
# one_occur_words_2 <- apply(tab_bl_nd_filtered, 2, function(x) return(sum(x[x!=0])))
# tab2_bl_nd <- tab_bl_nd_filtered[, which(one_occur_words_2 > 2)]
# tab2_bl_nd <- tab2_bl_nd[, which(nchar(colnames(tab2_bl_nd)) > 2)]
# topic_clus <- maptpx::topics(tab2_bl_nd, K=2, tol = 1)

topic_clus <- get(load("../output/CountClust_wo_negro_black_k_2_BL_ND_cutoff_2.rda"))
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

topic_clus <- get(load("../output/CountClust_wo_negro_black_k_2_Ebony_BL_ND_cutoff_2.rda"))

omega <- topic_clus$omega
annotation <- data.frame(
  sample_id = paste0("X", c(1:NROW(omega))),
  tissue_label = factor(rownames(omega),
                        levels = rownames(omega)))

rownames(omega) <- annotation$sample_id;

StructureGGplot(omega = omega,
                annotation = annotation,
                palette = RColorBrewer::brewer.pal(8, "Accent")[5:6],
                yaxis_label = "Years of Publication",
                order_sample = TRUE,
                axis_tick = list(axis_ticks_length = .1,
                                 axis_ticks_lwd_y = .1,
                                 axis_ticks_lwd_x = .1,
                                 axis_label_size = 7,
                                 axis_label_face = "bold"))

out <- ExtractTopFeatures(topic_clus$theta, top_features = 50, method = "poisson", options = "min")
driving_words <- apply(out$indices, c(1,2), function(x) return(rownames(topic_clus$theta)[x]))


####################   Trends of top driving  words    ##################################













