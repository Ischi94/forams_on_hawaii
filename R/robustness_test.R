library(here)
library(tidyverse)
library(brms)


# set up ------------------------------------------------------------------


# load raw data with foram index and distance to settlements
dat_fi <- read_csv(here("data/raw_fi.csv"))

# define function to calculate z_score standardization
z_score <- function(x){
  x <- scale(x)
  z <- as.numeric(x)
  return(z)
}

# remove potentially biased samples and standardize data
dat_fi_std <- dat_fi %>% 
  filter(sample %in% 6:13) %>% 
  mutate(across(depth:dist_mcbh, z_score))



# model fitting -----------------------------------------------------------

# fit model on subset
m3 <- brm(
  data = dat_fi_std, family = gaussian, 
  fi ~ 1 + dist_kan + dist_kah + dist_mcbh, 
  prior = c(prior(normal(0, 0.2), class = Intercept),
            prior(normal(0, 0.5), class = b),
            prior(cauchy(0, 1), class = sigma)), 
  iter = 2000, warmup = 500, chains = 4, 
  cores = 8, seed = 5, 
  file = here("model/robust_model"))

# reference model
m1 <- read_rds(here("model/distance_model.rds"))

# analyze results ---------------------------------------------------------

# get posterior samples for both models
samples_m3 <- posterior_samples(m3) %>% 
  as_tibble() %>% 
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
  filter(estimate %in% c("b_dist_kah", "b_dist_kan", "b_dist_mcbh")) %>% 
  mutate(estimate = c(1:3)) %>% 
  select(-c(ci_outer, ci_inner))

samples_m1 <- posterior_samples(m1) %>% 
  as_tibble() %>% 
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
  filter(estimate %in% c("b_dist_kah", "b_dist_kan", "b_dist_mcbh")) %>% 
  mutate(estimate = c(1:3)) %>% 
  select(-c(ci_outer, ci_inner))

# plot coefficients
robustness_coeff_plot <- samples_m1 %>% 
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
  geom_segment(aes(x = ci_inner_low, xend = ci_inner_high, 
                   y = estimate - 0.2, yend = estimate - 0.2), 
               size = 2.3, colour = "grey40", 
               alpha = 0.8, 
               data = samples_m3) +
  geom_segment(aes(x = ci_outer_low, xend = ci_outer_high,
                   y = estimate - 0.2, yend = estimate - 0.2), 
               size = 0.8, colour = "grey40", 
               alpha = 0.8, 
               data = samples_m3) +
  geom_point(aes(x = median_val, y = estimate - 0.2), 
             shape = 21, size = 5, 
             fill = "grey40", colour = "grey30", 
             stroke = 1, 
             data = samples_m3) +
  scale_y_continuous(breaks = c(0.9, 1.9, 2.9), 
                     labels = c("Distance Kāneʻohe", 
                                "Distance Kahaluʻu", 
                                "Distance MCBH")) +
  labs(y = NULL, x = "Coefficient estimate") +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.ticks = element_line())

# save image
ggsave("robustness_coeff_plot", plot = robustness_coeff_plot,
       device = "png", path = here("figures"))
