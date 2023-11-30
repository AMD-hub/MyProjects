library(readr)
Tragets <- read_delim("Tragets.txt", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

library(ggplot2)
library(ggthemes)
library(tidyverse)

x= Tragets %>% pivot_longer(-time,names_to = "Methode",values_to = "Estimation" ) 

ggplot(x) +
  aes(
    x = time,
    y = Estimation,
    colour = Methode,
    group = Methode
  ) +
  geom_line() +
  scale_color_viridis_d(option = "viridis", direction = 1) +
  theme_bw()


library(readr)
Erreurs <- read_delim("Erreurs.txt", delim = ";", 
                      escape_double = FALSE, trim_ws = TRUE)

xy= Erreurs %>% pivot_longer(-Number,names_to = "Methode",values_to = "Erreur_maximal" ) 

ggplot(xy) +
  aes(
    x = Number,
    y = Erreur_maximal,
    colour = Methode,
    group = Methode
  ) +
  geom_point(shape = "circle", size = 1.5) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  scale_color_viridis_d(option = "viridis", direction = 1) +
  theme_bw()
