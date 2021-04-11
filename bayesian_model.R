library(here)
library(tidyverse)
library(brms)
library(bayesplot)
library(ggridges)

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
  mutate(across(depth:dist_mcbh, z_score))


# model fitting -----------------------------------------------------------


# fit null model for comparison 
m0 <- brm(
  data = dat_fi_std, family = gaussian, 
  fi ~ 1, 
  prior = c(prior(normal(0, 0.2), class = Intercept), 
            prior(cauchy(0, 1), class = sigma)), 
            iter = 2000, warmup = 500, chains = 4, 
            cores = 8, seed = 5, file = here("model/null_model"))

# fit distance only model
m1 <- brm(
  data = dat_fi_std, family = gaussian, 
  fi ~ 1 + dist_kan + dist_kah + dist_mcbh, 
  prior = c(prior(normal(0, 0.2), class = Intercept),
            prior(normal(0, 0.5), class = b),
            prior(cauchy(0, 1), class = sigma)), 
  iter = 2000, warmup = 500, chains = 4, 
  cores = 8, seed = 5, file = here("model/distance_model"))

# fit distance and depth model
m2 <- brm(
  data = dat_fi_std, family = gaussian, 
  fi ~ 1 + dist_kan + dist_kah + dist_mcbh + depth, 
  prior = c(prior(normal(0, 0.2), class = Intercept),
            prior(normal(0, 0.5), class = b),
            prior(cauchy(0, 1), class = sigma)), 
  iter = 2000, warmup = 500, chains = 4, 
  cores = 8, seed = 5, file = here("model/distance_depth_model"))

# model comparison --------------------------------------------------------

# add LOO as criterion
m0 <- add_criterion(m0, criterion = "loo")

m1 <- add_criterion(m1, criterion = "loo")

m2 <- add_criterion(m2, criterion = "loo")

# compare models
comp <- loo_compare(m0, m1, m2, criterion = "loo")

print(comp, digits = 2, simplify = FALSE)


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





# analyze results ---------------------------------------------------------

ridge_plot <- posterior_samples(m1) %>% 
  as_tibble() %>% 
  select("Distance Kan'eohe" = b_dist_kan, 
         "Distance Kahalu'u" = b_dist_kah, 
         "Distance MCBH" = b_dist_mcbh) %>% 
  pivot_longer(cols = everything(), 
               names_to = "estimate") %>% 
  ggplot(aes(value, estimate, fill = estimate)) +
  stat_density_ridges(quantile_lines = TRUE, 
                      quantiles = 2, 
                      scale = 2.5, rel_min_height = 0.001, 
                      colour = "white", alpha = 0.7) +
  scale_fill_manual(values = c("steelblue", "firebrick", "darkgreen")) +
  theme_minimal()

posterior_samples(m1) %>% 
  as_tibble() %>% 
  select("Distance Kan'eohe" = b_dist_kan, 
         "Distance Kahalu'u" = b_dist_kah, 
         "Distance MCBH" = b_dist_mcbh) %>% 
  pivot_longer(cols = everything(), 
               names_to = "estimate") %>% 
  group_by(estimate) %>% 
  summarise(median_val = median(value), 
            ci_outer = list(rethinking::PI(value, prob = 0.95)), 
            ci_inner = list(rethinking::PI(value, prob = 0.89))) %>% 
  mutate(ci_outer_low = map_dbl(ci_outer, pluck(1)), 
         ci_outer_high = map_dbl(ci_outer, pluck(2)), 
         ci_inner_low = map_dbl(ci_inner, pluck(1)), 
         ci_inner_high = map_dbl(ci_inner, pluck(2))) %>% 
  ggplot(aes(y = estimate, yend = estimate)) +
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             colour = "grey10") +
  geom_segment(aes(x = ci_inner_low, xend = ci_inner_high), 
               size = 2.3, colour = "firebrick", 
               alpha = 0.8) +
  geom_segment(aes(x = ci_outer_low, xend = ci_outer_high), 
               size = 0.8, colour = "firebrick", 
               alpha = 0.8) +
  geom_point(aes(x = median_val), 
             shape = 21, size = 5, 
             fill = "steelblue", colour = "grey30", 
             stroke = 1) +
  labs(y = NULL, x = "Coefficient estimate") +
  theme_minimal()
