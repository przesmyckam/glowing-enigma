library(dplyr)
library(broom)
set.seed(1410)
dat2 <- cbind(rnorm(n = 50*24, mean = 0) %>% 
                matrix(nrow = 24) %>% 
                data.frame,
              rnorm(n = 50*24, mean = 1.5) %>% 
                matrix(nrow = 24) %>% 
                data.frame)
ncol(dat2)
dat2[,"status"]
lista<-lapply(paste0("X", 1L:100), function(i,j)
  t.test(x = dat2[, i], y = dat2[, j]) %>% 
    tidy %>% 
    mutate(param = i) %>% 
    select(param=param, p.value=p.value)
) %>% bind_rows() %>% 
  mutate(ap.value = p.adjust(p.value, method = "BY"))
#powiekszyc zbior parametrow, 24 pacjenrow w obu grupach, 100 parametrow, 50 istotnych par, 50 nieist, 
#losujemy seed
#nowedat, p.value
lista




raw_dat <-cbind(rbind(rnorm(n = 50*24, mean = 0) %>% 
                    matrix(nrow = 24) %>% 
                    data.frame,
                  rnorm(n = 50*24, mean = 1.5) %>% 
                    matrix(nrow = 24) %>% 
                    data.frame),
            rnorm(n = 100*24, mean = 0) %>% 
              matrix(nrow = 48) %>% 
              data.frame)

colnames(raw_dat) <- paste0("X", 1L:100)

dat <- mutate(raw_dat, status = c(rep("A", 24), rep("B", 24)))
dim(dat)
lapply(paste0("X", 1L:100), function(i)
  t.test(x = dat[dat[["status"]] == "A", i], y = dat[dat[["status"]] == "B", i]) %>% 
    tidy %>% 
    mutate(param = i) %>% 
    select(param, p.value)
) %>% bind_rows() %>% 
  mutate(ap.value = p.adjust(p.value, method = "BY"))
#ile z pwartsoc istotnych, ile nie, rozklad p wartosci dla obu grup parametrów

dat1 <- t(rbind(rnorm(n = 240, mean = 0) %>% 
                  matrix(nrow = 10) %>% 
                  data.frame %>% 
                  mutate(status = "A"),
                rnorm(n = 240, mean = 1.5) %>% 
                  matrix(nrow = 10) %>% 
                  data.frame %>% 
                  mutate(status = "B")))

lapply(paste0("X", 1L:24), function(i)
  t.test(x = dat[dat[["status"]] == "A", i], y = dat[dat[["status"]] == "B", i]) %>% 
    tidy %>% 
    mutate(param = i) %>% 
    select(param, p.value)
) %>% bind_rows() %>% 
  mutate(ap.value = p.adjust(p.value, method = "BY"))
#ile z pwartsoc istotnych, ile nie, rozklad p wartosci dla obu grup parametrów


dat1 <- rbind(rnorm(n = 240, mean = 0) %>% 
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
#ile z pwartsoc istotnych, ile nie, rozklad p wartosci dla obu grup parametrów


