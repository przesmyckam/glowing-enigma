library(BEST)
library(pbapply)

# ns - number of samples in each block (A or B)

random_samples <- function(ns) {
  rbind(data.frame(block_id = paste0("A", unlist(lapply(1L:ns, rep, times = 10))), 
                   values = rnorm(10*ns, mean = 0, sd = 1)),
        data.frame(block_id = paste0("B", unlist(lapply(1L:ns, rep, times = 10))), 
                   values = rnorm(10*ns, mean = 1, sd = 1)))
}

do_simulation <- function(ns) {
  randomed_samples <- random_samples(ns)
  
  all_combn <- combn(levels(randomed_samples[["block_id"]]), 2, simplify = FALSE)
  
  res_t <- do.call(rbind, pblapply(1L:length(all_combn), function(ith_combn_id) {
    ith_combn <- all_combn[[ith_combn_id]]
    best_res <- suppressMessages(BESTmcmc(randomed_samples[randomed_samples[["block_id"]] == ith_combn[1], "values"],
                                          randomed_samples[randomed_samples[["block_id"]] == ith_combn[2], "values"]))
    
    complete_res <- data.frame(gr1 = ith_combn[1],
                               gr2 = ith_combn[2],
                               same = substr(ith_combn[1], 0, 1) == substr(ith_combn[2], 0, 1),
                               effSz = summary(best_res)["effSz", "mean"], 
                               p.value = t.test(randomed_samples[randomed_samples[["block_id"]] == ith_combn[1], "values"],
                                                randomed_samples[randomed_samples[["block_id"]] == ith_combn[2], "values"], 
                                                paired = FALSE)[["p.value"]])
    
    write.csv(complete_res, file = paste0("/home/michal/Dropbox/bayes-sim/res", ith_combn_id, ".csv"), row.names = FALSE)
  }))
}

set.seed(15390)

res <- do_simulation(5)
