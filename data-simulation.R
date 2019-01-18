library(dplyr)
library(broom)
library(BEST)
set.seed(1410)

dat1 <- rbind(rnorm(n = 240, mean = 0) %>% 
                matrix(nrow = 10) %>% 
                data.frame %>% 
                mutate(status = "A"),
              rnorm(n = 240, mean = 1.5) %>% 
                matrix(nrow = 10) %>% 
                data.frame %>% 
                mutate(status = "B"))

dat_different <- lapply(paste0("X", 1L:24), function(i) {
  best_res <- BESTmcmc(y1 = dat[dat[["status"]] == "A", i], y2 = dat[dat[["status"]] == "B", i])
  eff_size <- summary(best_res)["effSz", "mean"]
  t.test(x = dat[dat[["status"]] == "A", i], y = dat[dat[["status"]] == "B", i]) %>% 
    tidy %>% 
    mutate(param = i) %>% 
    select(param, p.value) %>% 
    mutate(eff_size = eff_size)
}) %>% bind_rows() %>% 
  mutate(ap.value = p.adjust(p.value, method = "BY"))

dat_same <- lapply(paste0("X", 1L:24), function(i) {
  best_res <- BESTmcmc(y1 = dat[dat[["status"]] == "A", i], y2 = resample(dat[dat[["status"]] == "A", i]))
  eff_size <- summary(best_res)["effSz", "mean"]
  t.test(x = dat[dat[["status"]] == "A", i], y = resample(dat[dat[["status"]] == "A", i])) %>% 
    tidy %>% 
    mutate(param = i) %>% 
    select(param, p.value) %>% 
    mutate(eff_size = eff_size)
}) %>% bind_rows() %>% 
  mutate(ap.value = p.adjust(p.value, method = "BY"))


