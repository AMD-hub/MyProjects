library(tidyverse)



datapath = "C:/Users/DELL/Desktop/data_exam.csv"

data = read_delim(datapath, 
                  delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                  trim_ws = TRUE)




returns <-  data   %>% 
  mutate(across(everything(),function(col){(col-lag(col))/lag(col)} ) ) %>% 
  na.omit()


performance = returns %>% pivot_longer(everything(),values_to = "Return",names_to = "Asset" ) %>% 
  group_by(Asset) %>% 
  summarise(mean_return = 12*mean(Return),perf_return = prod(Return+1)-1)


Sigma = returns %>%
  cov() %>% `rownames<-`(colnames(returns) )

performance <- performance %>%
  arrange(match(Asset, colnames(Sigma)))



