library(readr)

library(tidyverse)
library(lubridate)
library(ggthemes)


CranckTest <- read_delim("CranckTest.txt", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)


Toplot = CranckTest %>% 
  pivot_longer(cols = c(`u(t,x)`,`estim u(t,x)`) , names_to = "Methode", values_to = "u(t,x)")




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


