library(readr)

library(tidyverse)
library(lubridate)
library(ggthemes)



BS <- read_delim("BS.txt", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)


Toplot = BS %>% 
  pivot_longer(cols = c(`y(t,x)`,`estim y(t,x)`) , names_to = "Methode", values_to = "u(t,x)")

BS %>% filter(t==0)


ggplot(Toplot) +
  aes(x = t, y = `u(t,x)`, colour = x, group = x) +
  geom_line() +
  scale_color_gradient() +
  theme_bw() +
  facet_wrap(vars(Methode))


ggplot(Toplot) +
  aes(x = x, y = `u(t,x)`, colour = t, group = t) +
  geom_line() +
  scale_color_gradient() +
  theme_bw() +
  facet_wrap(vars(Methode))

