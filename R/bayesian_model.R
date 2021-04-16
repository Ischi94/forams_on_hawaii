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

# standardize data for improved model fit and prior selection
dat_fi_std <- dat_fi %>% 
  mutate(across(depth:dist_mcbh, z_score))

# quick plot of foram index per sample
fi_plot <- dat_fi %>% 
  mutate(sample = as_factor(sample), 
         sample = fct_reorder(sample, desc(fi))) %>% 
  ggplot(aes(sample, fi)) +
  geom_hline(yintercept = c(2, 4), 
             linetype = "dashed", 
             colour = "firebrick", 
             alpha = 0.8) +
  geom_segment(aes(x = sample, xend = sample,
                   y = 0, yend = fi), 
               colour = "steelblue", size = 1) +
  geom_point(shape = 21, size = 7, 
             fill = "steelblue", colour = "grey30", 
             stroke = 1) +
  labs(y = "Foram Index", x = "Samples") +
  annotate(geom = "text", x = 0.65, y = 1, label = "no reefs", 
           angle = 90) +
  annotate(geom = "text", x = 0.65, y = 3, label = "marginal", 
           angle = 90) +
  annotate(geom = "text", x = 0.65, y = 7, label = "supports reefs", 
           angle = 90) +
  annotate(geom = "text", x = 12.5, y = 10, 
           label = paste("median =", median(dat_fi$fi))) +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
              axis.ticks = element_line())
  


# model fitting -----------------------------------------------------------


# fit null model for comparison 
m0 <- brm(
  data = dat_fi_std, family = gaussian, 
  fi ~ 1, 
  prior = c(prior(normal(0, 0.2), class = Intercept), 
            prior(cauchy(0, 1), class = sigma)), 
            iter = 2000, warmup = 500, chains = 4, 
            cores = 8, seed = 5, 
  file = here("model/null_model"))

# fit distance only model
m1 <- brm(
  data = dat_fi_std, family = gaussian, 
  fi ~ 1 + dist_kan + dist_kah + dist_mcbh, 
  prior = c(prior(normal(0, 0.2), class = Intercept),
            prior(normal(0, 0.5), class = b),
            prior(cauchy(0, 1), class = sigma)), 
  iter = 2000, warmup = 500, chains = 4, 
  cores = 8, seed = 5, 
  file = here("model/distance_model"))

# fit distance and depth model
m2 <- brm(
  data = dat_fi_std, family = gaussian, 
  fi ~ 1 + dist_kan + dist_kah + dist_mcbh + depth, 
  prior = c(prior(normal(0, 0.2), class = Intercept),
            prior(normal(0, 0.5), class = b),
            prior(cauchy(0, 1), class = sigma)), 
  iter = 2000, warmup = 500, chains = 4, 
  cores = 8, seed = 5, 
  file = here("model/distance_depth_model"))


# model comparison --------------------------------------------------------

# add LOO as criterion
m0 <- add_criterion(m0, criterion = "loo")

m1 <- add_criterion(m1, criterion = "loo")

m2 <- add_criterion(m2, criterion = "loo")

# compare models
comp <- loo_compare(m0, m1, m2, criterion = "loo")

as_tibble(comp, digits = 2, simplify = TRUE) 

comp %>%  
  as_tibble() %>% 
  select("ELPD difference" = elpd_diff, 
         "SE ELPD difference" = se_diff, 
         "Absolute ELPD" = elpd_loo, 
         "SE absolute ELPD" = se_elpd_loo) %>% 
  mutate(across(everything(), as.double), 
         across(everything(), round, 3)) %>% 
  write_csv(file = here("data/model_comparison.csv")) 


# model check -------------------------------------------------------------


# effective number of samples

neff_plot <- neff_ratio(m1) %>% 
  enframe(name = "estimate", value = "neff_ratio") %>% 
  filter(!str_detect(estimate, 'prior')) %>% 
  mutate(estimate = c("Intercept", 
                      "Distance Kāneʻohe", 
                      "Distance Kahaluʻu", 
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
  geom_point(shape = 21, size = 6, 
             fill = "steelblue", colour = "grey30", 
             stroke = 1) +
  labs(y = NULL, x = "Effective sample size ratio") +
  coord_cartesian(xlim = c(0, 0.75)) +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.ticks = element_line())

# rhat values
rhat_plot <- rhat(m1) %>% 
  enframe(name = "estimate", value = "rhat") %>% 
  filter(!str_detect(estimate, 'prior')) %>% 
  mutate(estimate = c("Intercept", 
                      "Distance Kāneʻohe", 
                      "Distance Kahaluʻu", 
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
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.ticks = element_line())

# trace plot for convergence
trace_plot <- mcmc_trace(m1) +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.ticks = element_line())
  


# trace plot for convergence
trank_plot <- mcmc_rank_overlay(m1) +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.ticks = element_line())


# analyze results ---------------------------------------------------------

ridge_plot <- posterior_samples(m1) %>% 
  as_tibble() %>% 
  select("Distance Kāneʻohe" = b_dist_kan, 
         "Distance Kahaluʻu" = b_dist_kah, 
         "Distance MCBH" = b_dist_mcbh) %>% 
  pivot_longer(cols = everything(), 
               names_to = "estimate") %>% 
  ggplot(aes(value, estimate, fill = estimate)) +
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             colour = "grey10") +
  stat_density_ridges(quantile_lines = TRUE, 
                      quantiles = 2, 
                      scale = 2.5, rel_min_height = 0.001, 
                      colour = "white", alpha = 0.7, 
                      show.legend = FALSE) +
  scale_fill_manual(values = c("steelblue", "firebrick", "darkgreen")) +
  labs(y = NULL, x = "Coefficient estimate") +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.ticks = element_line())

coeff_plot <- posterior_samples(m1) %>% 
  as_tibble() %>% 
  select("Distance Kāneʻohe" = b_dist_kan, 
         "Distance Kahaluʻu" = b_dist_kah, 
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
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.ticks = element_line())



regression_plot <- tibble(dist_kan = c(-2, 2.5), dist_kah = 0, dist_mcbh = 0) %>% 
  predict(m1, newdata = ., summary = FALSE, nsamples = 2000) %>% 
  as_tibble() %>% 
  set_names(c(-2, 2.5)) %>% 
  pivot_longer(cols = everything(), 
               names_to = "dist_kan", values_to = "fi") %>% 
  mutate(dist_kan = as.double(dist_kan)) %>% 
  add_column(iter = rep(1:2000, each = 2)) %>% 
  mutate(dist_kan = dist_kan * sd(dat_fi$dist_kan) + mean(dat_fi$dist_kan)) %>%
  ggplot(aes(dist_kan, fi)) +
  geom_line(aes(group = iter),
            colour = "firebrick", 
            size = 1, alpha = 0.01) +
  geom_line(colour = "firebrick",
            size = 1.5,
            data = tibble(dist_kan = c(-2, 2.5), dist_kah = 0, dist_mcbh = 0) %>%
              predict(m1, newdata = .) %>%
              as_tibble() %>%
              rename(fi = Estimate) %>%
              add_column(dist_kan = c(-2, 2.5)) %>%
              mutate(dist_kan = dist_kan * sd(dat_fi$dist_kan) + mean(dat_fi$dist_kan))) +
  geom_point(shape = 21, size = 4, 
             fill = "steelblue", colour = "grey30", 
             stroke = 1,
             data = dat_fi_std %>% 
               mutate(Estimate = fi, 
                      dist_kan = dist_kan * sd(dat_fi$dist_kan) + mean(dat_fi$dist_kan))) +
  labs(y = "Foram Index (std)", x = "Distance Kāneʻohe (km)") +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.ticks = element_line())



# save images -------------------------------------------------------------

# select all plots from environment
gg_charts <- ls()[str_detect(ls(),"plot")] 

all_plots <- mget(gg_charts)

# set image names
gg_names <- paste0(gg_charts, ".png")

# save all images at once
walk2(gg_names, all_plots, ~ggsave(filename = .x, plot = .y, 
                                   device = "png", 
                                   path = here("figures")))

# save images for manuscript as eps
cairo_ps(here("figures/coeff_plot.eps"), 
         width = 9.489583, height = 6.427083)
coeff_plot
dev.off()

cairo_ps(here("figures/regression_plot.eps"), 
         width = 9.489583, height = 6.427083)
regression_plot
dev.off()



