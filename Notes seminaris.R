library(tidyverse)
library(readxl)
library(janitor)

sem <- read_xlsx("seminar.xlsx") |> 
  clean_names() |> 
  filter(puntos_finales_transformed > 2) |> 
  mutate(seminar = as.character(seminar),
         seminar = fct_relevel(seminar, c("202", "204", "103", "101", 
                                          "203", "201", "104", "102")))

sem |> 
  ggplot(aes(x = puntos_finales_transformed)) +
  geom_histogram()

sem |> 
  ggplot(aes(x = seminar, y = puntos_finales_transformed)) +
  geom_point() +
  stat_summary(col = "red")


