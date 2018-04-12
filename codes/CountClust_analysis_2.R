
###########  CountClust without blacks and negros   ######################

tab <- get(load("../output/table_word_frequencies.rda"))
tab_data_frame <- data.frame(tab)
colnames(tab_data_frame) <-  colnames(tab)
tab2_data_frame <- tab_data_frame[, !(colnames(tab_data_frame) %in% c("black", "blacks", "africa", "afrika", 
                                       "african", "africans", "afrikan", "negro",
                                       "negros", "negroes", "antinegro"))]
one_occur_words <- apply(tab2_data_frame, 2, function(x) return(sum(x[x!=0])))
mat <- tab2_data_frame[, which(one_occur_words > 2)]
topic_clus <- maptpx::topics(mat, K=4, tol = 1)
save(topic_clus, file = "../output/CountClust_k_4_cutoff_2_wo_negro_black.rda")

topic_clus <- maptpx::topics(mat, K=3, tol = 1)
save(topic_clus, file = "../output/CountClust_k_3_cutoff_2_wo_negro_black.rda")


topic_clus <- get(load("../output/CountClust_k_2_cutoff_2_wo_negro_black.rda"))
library(CountClust)
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


topic_clus <- get(load("../output/CountClust_k_3_cutoff_2_wo_negro_black.rda"))
library(CountClust)
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
