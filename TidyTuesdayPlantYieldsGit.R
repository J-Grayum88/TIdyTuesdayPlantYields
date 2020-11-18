library(tidyverse)
library(tidytuesdayR)
library(janitor)
library(tidymodels)
#install.packages("ggrepel")
library(ggrepel)
theme_set(theme_light())

tuesdata <- tidytuesdayR::tt_load('2020-09-01')

key_crop_yields <- tuesdata$key_crop_yields
land_use <- tuesdata$land_use_vs_yield_change_in_cereal_production


land_use %>%
  glimpse()

land_use %>%
  view()

land_use %>%
  distinct(Code, sort = TRUE)

top_countries <- land_use %>%
  janitor::clean_names() %>%
  filter(!is.na(code),
         entity != "World") %>%
  group_by(entity) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  slice_max(total_population_gapminder, n = 30) %>%
  pull(entity)

tidy_yields <- key_crop_yields %>%
  janitor::clean_names() %>%
  pivot_longer(wheat_tonnes_per_hectare:bananas_tonnes_per_hectare, 
               names_to = "crop", 
               values_to = "yield") %>%
  mutate(crop = str_remove(crop, "_tonnes_per_hectare")) %>%
  filter(crop %in% c("wheat", "rice", "maize", "barley"),
         entity %in% top_countries,
         !is.na(yield)) 

tidy_yields %>%
  ggplot(aes(year, yield, color = crop)) +
  geom_line(alpha = 0.7, size = 1.5) +
  geom_point() +
  facet_wrap(~ entity) +
  labs(x = "", y = "Yield in tonnes per hectare")

#First, we nest the data, and run our formula (lm(yield ~ year, data = .x)) over that using the fx map.
tidy_lm <- tidy_yields %>%
  nest(yields = c(year, yield)) %>%
  mutate(model = map(yields, ~ lm(yield ~ year, data = .x)))

#Now lets get rid of some coefficients and put them in TidyFormat, then unnest.
slopes <- tidy_lm %>%
  mutate(coefs = map(model, tidy)) %>%
  unnest(coefs) %>%
  filter(term == "year") %>%
  mutate(p.value = p.adjust(p.value))


slopes %>%
  ggplot(aes(estimate, p.value, label = entity)) +
  geom_vline(xintercept = 0, lty = 2, size = 1.5, alpha = .7, color = "gray50") +
  geom_point(aes(color = crop), alpha = 0.8, size = 2.5, show.legend = FALSE) +
  geom_text_repel(size = 2.5) +
  facet_wrap(~ crop) +
  scale_y_log10()




