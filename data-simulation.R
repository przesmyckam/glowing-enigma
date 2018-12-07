library(dplyr)
library(broom)

dat <- rbind(rnorm(n = 240, mean = 0) %>% 
               matrix(nrow = 10) %>% 
               data.frame %>% 
               mutate(status = "A"),
             rnorm(n = 240, mean = 1.5) %>% 
               matrix(nrow = 10) %>% 
               data.frame %>% 
               mutate(status = "B"))

lapply(paste0("X", 1L:24), function(i)
  t.test(x = dat[dat[["status"]] == "A", i], y = dat[dat[["status"]] == "B", i]) %>% 
    tidy %>% 
    mutate(param = i) %>% 
    select(param, p.value)
) %>% bind_rows() %>% 
  mutate(ap.value = p.adjust(p.value, method = "BY"))


