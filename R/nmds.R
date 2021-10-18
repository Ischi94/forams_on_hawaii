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

nmds_plot <- tibble(nmds1 = scores(nmds)[,1], 
       nmds2 = scores(nmds)[,2], 
       samples = dat_assemblage$sample) %>% 
  mutate(group = if_else(nmds1 < 0 & nmds2 < 0.2, "Group 1", 
                         if_else(nmds2 > 0, "Group 2", "Group 3"))) %>% 
  ggplot(aes(nmds1, nmds2, group = group)) +
  geom_mark_ellipse(aes(fill = group), 
                             color = "white") +
  geom_label(aes(label = samples), 
             nudge_y = c(rep(0, 12), 0.03)) +
  annotate("rect", xmin = -0.53, xmax = -0.35, ymin = 0.49, ymax = 0.52, 
           fill = "white", 
           colour = "white") +
  annotate("text", x = -0.43, y = 0.505, 
           label = paste("stress = ", round(nmds$stress, 3)), 
           size = 4.5) +
  coord_cartesian(ylim = c(-0.6, 0.5), 
                  xlim = c(-0.5, 0.7)) +
  scale_fill_manual(values = c("darkgreen", "firebrick", "steelblue")) +
  labs(x = "NMDS 1", y = "NMDS 2") +
  theme_minimal(base_size = 14) +
  theme(axis.ticks = element_line(), 
        legend.position = "none")

# save image
ggsave("nmds_plot", plot = nmds_plot,
       device = "png", path = here("figures"))
