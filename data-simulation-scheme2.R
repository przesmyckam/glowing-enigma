library(BEST)

# ns - number of samples in each block (A or B)

random_samples <- function(ns) {
  rbind(data.frame(block_id = paste0("A", unlist(lapply(1L:ns, rep, times = 10))), 
                   values = rnorm(10*ns, mean = 0, sd = 1)),
        data.frame(block_id = paste0("B", unlist(lapply(1L:ns, rep, times = 10))), 
                   values = rnorm(10*ns, mean = 3, sd = 1)))
}

do_simulation <- function(ns) {
  randomed_samples <- random_samples(ns)
  
  all_combn <- combn(levels(randomed_samples[["block_id"]]), 2, simplify = FALSE)
  
  res_t <- do.call(rbind, lapply(all_combn, function(ith_combn) {
    
    best_res <- BESTmcmc(randomed_samples[randomed_samples[["block_id"]] == ith_combn[1], "values"],
                         randomed_samples[randomed_samples[["block_id"]] == ith_combn[2], "values"])
    
    data.frame(same = substr(ith_combn[1], 0, 1) == substr(ith_combn[2], 0, 1),
               effSz = summary(best_res)["effSz", "mean"], 
               p.value = t.test(randomed_samples[randomed_samples[["block_id"]] == ith_combn[1], "values"],
                                randomed_samples[randomed_samples[["block_id"]] == ith_combn[2], "values"], 
                                paired = FALSE)[["p.value"]])
  }))
  
  res_t[["hochberg"]] <- p.adjust(res_t[["p.value"]], method = "BH")
  res_t[["yekutieli"]] <- p.adjust(res_t[["p.value"]], method = "BY")
  res_t[["ns"]] <- ns
  res_t
}

whole_simultion <- lapply(2:3, do_simulation)
