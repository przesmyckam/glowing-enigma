library(dplyr)

full_dat <- lapply(list.files("/home/michal/Dropbox/bayes-sim/", full.names = TRUE),
       read.csv) %>% do.call(rbind, .) 

adjust_subset <- function(ns, x)
  lapply(seq(from = 10, to = ns, length.out = 20), function(ith_n) {
    ids <- c(sample(x = which(x[["same"]]), size = ith_n, replace = TRUE),
             sample(x = which(!x[["same"]]), size = ith_n, replace = TRUE))
    subset_x <- x[ids, ]
    subset_x[["hochberg"]] <- p.adjust(subset_x[["p.value"]], method = "BH")
    subset_x[["yekutieli"]] <- p.adjust(subset_x[["p.value"]], method = "BY")
    subset_x[["ns"]] <- ith_n
    subset_x[, c("same", "effSz", "hochberg", "yekutieli", "ns")]
  })

dat <- adjust_subset(1e5, full_dat) %>% 
  bind_rows() %>% 
  group_by(ns, same) %>% 
  summarise(mean_eff = mean(effSz), mean_hochberg = mean(hochberg))


library(ggplot2)

ggplot(dat, aes(x = ns, y = mean_hochberg, color = same)) +
  geom_point()


adjust_subset(1e5, full_dat) %>% 
  bind_rows() %>% 
  group_by(ns, same) %>% 
  summarise(mean_eff = mean(hochberg > 0.05)) %>% 
  data.frame()
