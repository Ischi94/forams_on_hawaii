library(here)
library(tidyverse)
library(brms)
library(bayesplot)


# set-up ------------------------------------------------------------------


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


# model fitting -----------------------------------------------------------


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


# model check -------------------------------------------------------------

# effective number of samples

neff_plot <- neff_ratio(m1) %>% 
  enframe(name = "estimate", value = "neff_ratio") %>% 
  mutate(estimate = c("Intercept", 
                      "Distance Kan'eohe", 
                      "Distance Kahalu'u", 
                      "Distance MCBH", 
                      "Sigma", 
                      "LP"), 
         estimate = as_factor(estimate), 
         estimate = fct_reorder(estimate, neff_ratio)) %>% 
  filter(estimate != "LP") %>% 
  ggplot(aes(neff_ratio, estimate)) +
  geom_vline(xintercept = 0.1, 
             colour = "firebrick", alpha = 0.4, 
             linetype = "dashed") +
  geom_segment(aes(x = 0, xend = neff_ratio, 
                   y = estimate, yend = estimate), 
               colour = "steelblue", size = 0.8) +
  geom_point(shape = 21, size = 3, 
             fill = "steelblue", colour = "grey30", 
             stroke = 1) +
  labs(y = NULL, x = "Effective sample size ratio") +
  coord_cartesian(xlim = c(0, 0.75)) +
  theme_minimal()

# rhat values
rhat_plot <- rhat(m1) %>% 
  enframe(name = "estimate", value = "rhat") %>% 
  mutate(estimate = c("Intercept", 
                      "Distance Kan'eohe", 
                      "Distance Kahalu'u", 
                      "Distance MCBH", 
                      "Sigma", 
                      "LP")) %>% 
  ggplot(aes(rhat, estimate)) +
  geom_point(shape = 21, size = 3, 
             fill = "steelblue", colour = "grey30", 
             stroke = 1) +
  geom_vline(xintercept = 1.05,
             colour = "firebrick", alpha = 0.4, 
             linetype = "dashed") +
  labs(y = NULL, x = "Rhat value") +
  theme_minimal()

# trace plot for convergence
trank_plot <- mcmc_rank_overlay(m1) +
  theme_minimal()


