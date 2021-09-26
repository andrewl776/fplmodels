
# EDA ---------------------------------------------------------------------
library(corrplot)
library(GGally)
library(tidyverse)

gw_data <- readr::read_rds("data/players_by_gameweek.rds")

gw_data %>%
  select(where(is.numeric)) %>%
  # select(-next_gw_points) %>%
  cor() %>%
  as.data.frame() %>%
  map(function(vec) sort(vec))

tiff("corrplot.tiff", units="in", width=5, height=5, res=300)
# insert ggplot code
gw_data %>%
  select(where(is.numeric)) %>%
  # select(-next_gw_points) %>%
  cor() %>%
  corrplot(order = "hclust", tl.cex = 0.4)
dev.off()

# There is a variable (ep_next) which


gw_data %>%
  filter(event_points != 0,
         chance_of_playing_next_round == 100) %>%
  group_by(next_gw_points) %>%
  summarise("n" = n()) %>%
  ggplot(aes(x = factor(next_gw_points), y = n)) +
  geom_bar(stat = "identity")

# CHARACTERS AND FACTORS

gw_data %>%
  mutate(across(where(is.character), as.factor)) %>%
  select(where(is.factor)) %>%
  select(-contains("name"), -contains("news"))

# WE HAVE LEARNT THAT CHARACTERS ARE USELESS!
gw_data %>%
  fplmodels:::delete_columns_with_many_NAs()

gw_data %>%
  filter(is.na(chance_of_playing_next_round)) %>%
  ggplot(aes(x = next_gw_points)) +
  geom_histogram()

t.test(data = gw_data %>% mutate(na_chance = is.na(chance_of_playing_next_round)),
       next_gw_points ~ na_chance)

gw_data %>%
  group_by(chance_of_playing_next_round) %>%
  summarise("mean" = mean(next_gw_points), "n" = n()) %>%
  arrange(-mean)

# When chance of playing is NA, just convert to 75% or 100% either is probably fine
gw_data %>%
  mutate(chance_of_playing_next_round =
           ifelse(is.na(chance_of_playing_next_round), 75, chance_of_playing_next_round),
         chance_100 = (chance_of_playing_next_round == 100)
  ) %>%
t.test(data = .,
       next_gw_points ~ chance_100)

# We want chance of playing to be 100.
gw_data %>%
  drop_na(chance_of_playing_this_round) %>%
  filter(chance_of_playing_this_round == 100) %>%
  ggplot(aes(x = next_gw_points)) +
  geom_histogram()

# Can we separate 0s from everything else
gw_data %>%
  select(where(is.numeric)) %>%
  mutate(zero_points = (next_gw_points == 0)) %>%
  pivot_longer(-zero_points, names_to = "attribute", values_to = "value") %>%
  ggplot(aes(x = value, fill = zero_points)) +
  geom_density(aes(alpha = I(0.5))) +
  facet_wrap(~attribute, scales = "free")


# Would it be useful anyway?
gw_data %>%
  filter(next_gw_points != 0) %>%
  ggplot(aes(x = next_gw_points)) +
  geom_histogram()

# Yes it likely would be useful


ggplot(mtcars, aes(x = mpg, fill = factor(4 - cyl))) +
  geom_histogram(aes(alpha = 0.5))



