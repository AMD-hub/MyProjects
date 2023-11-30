library(readr)
Comparaison <- read_delim("ComparingMethodes.txt", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

library(ggplot2)
library(ggthemes)
library(tidyverse)

x= Comparaison %>%
  select(Number,Naive_MC,Controle_MC,Anthetique_MC,Anthetique_Controle_MC) %>% 
  pivot_longer(-Number,names_to = "Methode",values_to = "Estimation" ) 

ggplot(x) +
  aes(
    x = Number,
    y = Estimation,
    colour = Methode,
    group = Methode
  ) +
  geom_line() +
  scale_color_viridis_d(option = "viridis", direction = 1) +
  scale_x_continuous(trans = "log10") +
  labs(title = "Monte Carlo methodes") +
  theme_bw()



xy= Comparaison %>%
  select(Number,Naive_Err,Controle_Err,Anthetique_Err,Anthetique_Controle_Err) %>% 
  pivot_longer(-Number,names_to = "Methode",values_to = "Amplitude d'IC" ) 

xy %>%
  filter(!(Methode %in% "Black_Formula")) %>%
  ggplot() +
  aes(
    x = Number,
    y =  `Amplitude d'IC` ,
    colour = Methode,
    group = Methode
  ) +
  geom_line() +
  scale_color_hue(direction = 1) +
  scale_x_continuous(trans = "log10") +
  labs(title = "Monte Carlo amplitudes d'IC") +
  theme_bw()

