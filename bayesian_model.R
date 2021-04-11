library(here)
library(tidyverse)
library(brms)

# load raw data with foram index and distance to settlements
dat_fi <- read_csv(here("data/raw_fi.csv"))

# define function to calculate z_score standardization
z_score <- function(x){
  x <- scale(x)
  z <- as.numeric(x)
  return(z)
}

# standardize data for improve model fit and prior selection
dat_fi_std <- dat_fi %>% 
  mutate(across(fi:dist_mcbh, z_score)) %>% 
  select(-depth)

# fit null model for comparison 
m0 <- brm(
  data = dat_fi_std, family = gaussian, 
  fi ~ 1, 
  prior = c(prior(normal(0, 0.2), class = Intercept), 
            prior(cauchy(0, 1), class = sigma)), 
            iter = 2000, warmup = 500, chains = 4, 
            cores = 8, seed = 5, file = here("model/null_model"))

# fit actual model
m1 <- brm(
  data = dat_fi_std, family = gaussian, 
  fi ~ 1 + dist_kan + dist_kah + dist_mcbh, 
  prior = c(prior(normal(0, 0.2), class = Intercept),
            prior(normal(0, 0.5), class = b),
            prior(cauchy(0, 1), class = sigma)), 
  iter = 2000, warmup = 500, chains = 4, 
  cores = 8, seed = 5, file = here("model/actual_model"))


