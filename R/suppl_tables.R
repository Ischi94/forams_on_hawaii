library(here)
library(tidyverse)
library(flextable)
library(officer)
library(brms)


# load data ---------------------------------------------------------------

# assemblage data
dat_assemblage <- read_csv(here("data/assemblage.csv")) %>% 
  janitor::clean_names()

# foram index and distance to settlements
dat_fi <- read_csv(here("data/raw_fi.csv"))

# final model
m1 <- read_rds(here("model/distance_model.rds"))




# tables ------------------------------------------------------------------

m1_smr <- m1 %>% 
  summary(prior = TRUE)

m1_smr$fixed %>% 
  as_tibble(rownames = "Coefficient")

