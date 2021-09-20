library(here)
library(tidyverse)

# assemblage data
dat_assemblage <- read_csv(here("data/assemblage.csv"))

# calculate foram index
dat_assemblage %>% 
  select(Sample, Amphistegina:Elphidium) %>% 
  mutate(across(Amphistegina:Elphidium, ~ .x /100)) %>% 
  mutate(s = Amphistegina + Heterostegina + Peneroplis + Alveolinida + Soritida, 
         o = Ammonia + Textulariida + Bolivinida + Elphidium) %>% 
  rename("h" = 'Other Miliolida') %>% 
  select(Sample, s, o, h) %>% 
  mutate(fi = (10*s) + o + (2*h))
