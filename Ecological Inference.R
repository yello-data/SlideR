## ECOLOGICAL INFERENCE PROBLEM
# More info: https://en.wikipedia.org/wiki/Simpson%27s_paradox

#Packages
library(dplyr)
library(tidyr)

# The Simpson's paradox
# Tibble with admissions to different undergraduate programmes
uni <- tibble(faculty = rep(c("Psysics", "Politics", "Journalism"), times = 1, each = 2),
       sex = rep(c("Men", "Women"), times = 3),
       applicants = c(825,108,325,593,181,392),
       admitted = c(520, 76, 119, 203, 52, 115))

uni


# What is aggregate data (uni level) telling us?
uni |> 
  group_by(sex) |> 
  summarize(applicants = sum(applicants),
            admitted = sum(admitted)) |> 
  mutate(perc_admissions = admitted / applicants * 100)

#But when we disaggregate the data faculty level ...
uni |> 
  mutate(perc_admissions = admitted / applicants * 100) |>
  arrange(desc(perc_admissions))

## DURKHEIM SUICIDE
suicide <- tibble(region = c("Isère"),
              municipality = rep(c("Grenoble", "Le Bourg-d'Oisans", "Saint-Jean-de-Maurienne"), times = 1, each = 2),
              religion = rep(c("Protestant", "Catholic"), times = 3),
              population = c(8250,1080,325,593,181,392),
              suicide = c(520, 72, 12, 20, 5, 11))

#What aggregate data (regional level) is telling us?
suicide |> 
  group_by(religion) |> 
  summarize(population = sum(population),
            suicide = sum(suicide)) |> 
  mutate(perc = suicide / population * 100)

#But when we disaggregate the data ...
suicide |> 
  mutate(perc = suicide / population * 100) |>
  arrange(desc(perc))




