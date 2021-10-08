library(here)
library(tidyverse)
library(flextable)
library(officer)



# load data ---------------------------------------------------------------

# assemblage data
dat_assemblage <- read_csv(here("data/assemblage.csv"))

# foram index and distance to settlements
dat_fi <- read_csv(here("data/raw_fi.csv"))

# final model
m1 <- read_rds(here("model/distance_model.rds"))

# model comparison
dat_comp <- read_csv(file = here("data/model_comparison.csv"))


# tables ------------------------------------------------------------------

# model summary
m1_smr <- m1 %>% 
  summary(prior = TRUE)

# convert to tidy flextable
m1_smr_fxt <- m1_smr$fixed %>% 
  as_tibble(rownames = "Coefficient") %>% 
  mutate(across(Estimate:Rhat, round, 3), 
         across(Bulk_ESS:Tail_ESS, as.integer), 
         Coefficient = c("Intercept",
                         "Distance Kāneʻohe",
                         "Distance Kahaluʻu", 
                         "Distance MCBH")) %>% 
  flextable() %>% 
  bold(part = "header") %>% 
  autofit()

# open docx-file and add flextable
my_doc <- read_docx() %>% 
  body_add_flextable(m1_smr_fxt)  


# model comparison
comp_fxt <- dat_comp %>% 
  flextable() %>% 
  bold(part = "header") %>% 
  autofit()


# add to word file
my_doc <- my_doc %>% 
  body_add_break() %>% 
  body_add_flextable(comp_fxt, pos = "after")


# assemblage data
assemblage_fxt <- dat_assemblage %>% 
  select(-c(Latitude,Longitude, Depth), "Absolute Abundance" = "absolute abundance") %>%
  rename("Other Small Taxa" = "Other Miliolida") %>% 
  mutate(across(everything(), round, 1),
         across(everything(), as.character),
         Sample = as.integer(Sample)) %>% 
  flextable() %>% 
  add_header_row(colwidths = c(1, 5, 1, 4, 1), 
                 values = c("", "Symbiont-bearing", "", "Opportunistic", "")) %>% 
  theme_booktabs(bold_header = TRUE) %>% 
  border_remove() %>% 
  vline(j = c(1, 6, 7, 11), part = "header",
        border = fp_border(color = "grey40", width = 1.25)) %>% 
  hline_top(part = "header", 
            j = 1:12, 
            border = fp_border(color = "grey40", width = 2.5)) %>% 
  hline_top(j = 1:12, 
            border = fp_border(color = "grey40", width = 2.5)) %>%
  hline_bottom(j = 1:12, border = fp_border(color = "grey40", width = 2.5)) %>% 
  align(align = "center", part = "all") %>% 
  autofit() 


# add to word file
my_doc <- my_doc %>% 
  body_add_break() %>% 
  body_add_flextable(assemblage_fxt, pos = "after")


# sample information
sample_fxt <- dat_fi %>% 
  rename("Sample" = sample) %>% 
  full_join(dat_assemblage) %>% 
  select(-c(Amphistegina:Elphidium), -"absolute abundance") %>% 
  select(Sample,
         Latitude, Longitude,
         Depth,
         -c(depth, fi), 
         "Distance Kāneʻohe" = dist_kan, 
         "Distance Kahaluʻu" = dist_kah, 
         "Distance MCBH" = dist_mcbh) %>% 
  mutate(Sample = as.integer(Sample)) %>% 
  flextable() %>% 
  colformat_double(j = c(2,3), 
                   digits = 0, big.mark = "") %>% 
  bold(part = "header") %>% 
  autofit()

# add to word file
my_doc <- my_doc %>% 
  body_add_break() %>% 
  body_add_flextable(sample_fxt, pos = "after")


# foram index
fi_fxt <- dat_fi %>% 
  select("Sample" = sample, fi) %>%
  add_column("Foram Index" = dat_fi$fi, .before = "fi") %>% 
  flextable() %>% 
  compose(j = 3,
          value = as_paragraph(
            linerange(value = fi, max = max(fi), height = .15)),
          part = "body") %>% 
  bold(part = "header") %>% 
  compose(j = "fi", part = "header", 
          value = as_paragraph("")) %>% 
  autofit()

# add to word file
my_doc <- my_doc %>% 
  body_add_break() %>% 
  body_add_flextable(fi_fxt, pos = "after")


# convert to word file/ add input to empty docx
print(my_doc, target = here("tables/extended_tables.docx"))


