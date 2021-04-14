library(here)
library(tidyverse)
library(vegan)
library(ggforce)

# load assemblage data
dat_assemblage <- read_csv(here("data/assemblage.csv")) %>% 
  janitor::clean_names()


# nmds --------------------------------------------------------------------


# Calculate distance matrix
distmat <- dat_assemblage %>% 
  select(-c(sample:longitude)) %>% 
  vegdist(method = "bray")

# conduct nmds
nmds <- metaMDS(distmat,
        distance = "bray",
        k = 2,
        maxit = 999, 
        trymax = 500)



# visualization -----------------------------------------------------------

tibble(nmds1 = scores(nmds)[,1], 
       nmds2 = scores(nmds)[,2], 
       samples = dat_assemblage$sample) %>% 
  mutate(group = if_else(nmds1 < 0 & nmds2 < 0.2, "Group 1", 
                         if_else(nmds2 > 0, "Group 2", "Group 3"))) %>% 
  ggplot(aes(nmds1, nmds2, group = group)) +
  geom_mark_ellipse(aes(fill = group), 
                             color = "white") +
  geom_label(aes(label = samples), 
             nudge_y = c(rep(0, 12), 0.03)) +
  coord_cartesian(ylim = c(-0.3, 0.5)) +
  scale_fill_manual(values = c("darkgreen", "firebrick", "steelblue")) +
  labs(x = "NMDS 1", y = "NMDS 2") +
  theme_minimal() +
  theme(axis.ticks = element_line(), 
        legend.position = "none")
