####  ~~~~~~~~~~~~~~~~~~~~~~~~  Tidy Tuesday ~~~~~~~~~~~~~~~~~~~~~~~~~~~   ####
##### 2026: week 1 

# 09/01/2026
# prompt: "Bring your own data"


# Inspired by a recent trip to Japan, I am looking at 

library(tidyverse)
library(janitor)
library(skimr)
library(tidytext)


getwd()

# import pokemon data

pokemon_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-01/pokemon_df.csv')
# save to data folder for reference
# write_csv(pokemon_df, "data/2026_wk1_pokemon.csv")


# data source / credits

# This dataset is sourced from {pokemon} (CRAN | github), an R package which provides Pokemon information in both English and Brazilian Portuguese.
# This package provides a dataset of Pokemon information in both English and Brazilian Portuguese. The dataset contains 949 rows and 22 columns, 
# including information such as the Pokemon’s name, ID, height, weight, stats, type, and more.
# Thank you to Frank Hull for curating this week's dataset.

# summarise, check for missing data

skim(pokemon_df)

# 949 records
# type_1 full records
# type_2 439 missing records: 54% completion rate.  Investigate? 

# initial groupings and visualisations
pokemon_df %>% group_by(type_1, type_2) %>% count()


# initial visualisation - sorting pokemon by type (1st Gen only)

pokemon_df %>% 
  filter(generation_id == 1) %>% 
  group_by(type_1, color_1) %>% count() %>% 
  ggplot()+
  geom_col(aes(x=n, y= reorder(type_1,n), fill = color_1))+
  scale_fill_identity()+
  labs(title = "Pokemon by type",
    subtitle = "first generation Pokemon only",
    y = "type",
    x = "count")+
  theme_minimal()+
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))


# sorting pokemon by type (beyond 1st gen)
pokemon_df %>% 
  filter(!is.na(generation_id)) %>% 
  group_by(type_1, color_1, generation_id) %>% 
  count() %>% 
  ggplot(aes(
    x = n,
    y = reorder_within(type_1, n, generation_id),
    fill = color_1
  )) +
  geom_col() +
  scale_fill_identity() +
  scale_y_reordered() +
  facet_wrap(~ generation_id, scales = "free_y") +
  labs(
    title = "Pokemon by type",
    subtitle = "by generation",
    y = "Type",
    x = "Count"
  )+
  theme_minimal()+
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))

# vital statistics of first gen Pokemon 


pokemon_df %>% 
  filter(generation_id == 1) %>% 
  select(pokemon,type_1, hp, height, weight) %>% 
  arrange(desc(height)) %>% 
  print(n=20)

pokemon_df %>% 
  filter(generation_id == 1) %>% 
  select(pokemon,type_1, hp, height, weight) %>% 
  arrange(desc(weight)) %>% 
  print(n=20)


pokemon_df %>% 
  filter(generation_id == 1) %>% 
  select(pokemon,type_1, hp, height, weight) %>% 
  arrange(height) %>% 
  print(n=20)

pokemon_df %>% 
  filter(generation_id == 1) %>% 
  select(pokemon,type_1, hp, height, weight) %>% 
  arrange(weight) %>% 
  print(n=20)


### explore

# BMI of pokemon?

# BMI is calculated by dividing an adult's weight in kg by their height in metres squared

# confirmed that dataset height = metres, and weight = kilograms

bmi_calc <- function(height, weight){
  weight / (height^2)
}

pokemon_df %>% 
  filter(generation_id == 1) %>% 
  mutate(bmi = bmi_calc(height, weight)) %>% 
  select(pokemon, bmi, type_1, hp, height, weight) %>% 
  arrange(desc(bmi)) %>% 
  print(n=20)
  