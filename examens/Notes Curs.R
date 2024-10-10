# Carreguem paquets i dades
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(janitor)
theme_set(theme_minimal())

notes_ac24 <- read_csv("Notes i Examen/notes_ac24r.csv") |> 
  clean_names() |>
  transmute(estudiant = paste(nom, cognoms),
            total_ac = total_de_avaluacio_continua_20_percent_real,
            moodle = total_de_test_moodle_real,
            dplyr = as.numeric(tasca_exercici_respondre_preguntes_amb_dplyr_real),
            ggplot = tasca_exercici_ggplot2_real,
            entregues = case_when(moodle > 0 & dplyr > 0 & ggplot > 0 ~ "Totes",
                              moodle > 0 | dplyr > 0 | ggplot > 0 ~ "Alguna",
                              TRUE ~ "Cap"))

notes_ac24 |> 
  mutate(estudiant = "XXXX XXXX") |> 
  sample_frac()

write_csv(notes, "Notes/notes.csv")

#1. ESTADÍSTICS DESCRIPTIUS

notes <- read_csv("data/notes.csv")

# 1.1. Inclòs 0
full_join(
  notes_ac24 |> 
    summarize(mitjana = mean(total_ac),
              mediana = median(total_ac),
              maxim = max(total_ac),
              minim = min(total_ac),
              aprovats = sum(total_ac >= 5),
              perc_aprovats = mean(total_ac >= 5) * 100,
              zeros = sum(total_ac == 0),
              n = n()) |> 
    pivot_longer(mitjana:n, names_to = "descriptius", values_to = "tot_curs"),
  notes_ac24 |> 
    filter(total_ac > 0) |> 
    summarize(mitjana = mean(total_ac),
              mediana = median(total_ac),
              maxim = max(total_ac),
              minim = min(total_ac),
              aprovats = sum(total_ac >= 5),
              perc_aprovats = mean(total_ac >= 5) * 100,
              zeros = sum(total_ac == 0),
              n = n()) |> 
    pivot_longer(mitjana:n, names_to = "descriptius", values_to = "alguna_entrega")
)


#1.3. Els que han entregat les 3 activitats
full_join(
  notes_ac24 |> 
    filter(entregues == "Totes") |> 
    summarize(mitjana = mean(total_ac),
              mediana = median(total_ac),
              maxim = max(total_ac),
              minim = min(total_ac),
              aprovats = sum(total_ac >= 5),
              perc_aprovats = mean(total_ac >= 5) * 100,
              zeros = sum(total_ac == 0),
              n = n()) |> 
    pivot_longer(mitjana:n, names_to = "descriptius", values_to = "totes"),
  notes_ac24 |> 
    filter(entregues == "Alguna") |> 
    summarize(mitjana = mean(total_ac),
              mediana = median(total_ac),
              maxim = max(total_ac),
              minim = min(total_ac),
              aprovats = sum(total_ac >= 5),
              perc_aprovats = mean(total_ac >= 5) * 100,
              zeros = sum(total_ac == 0),
              n = n()) |> 
    pivot_longer(mitjana:n, names_to = "descriptius", values_to = "alguna"),
)



#2. VISUALITZACIONS

#2.1. Histograma
notes_ac24 |> 
  ggplot(aes(x = total_ac)) +
  geom_histogram(bins = 20) +
  scale_x_continuous(breaks = 0:10) +
  geom_vline(xintercept = mean(notes_ac24$total_ac), col = "blue") +
  geom_vline(xintercept = median(notes_ac24$total_ac), col = "green")

#2.2. Histograma
notes_ac24 |> 
  filter(total_ac > 0) |> 
  ggplot(aes(x = total_ac)) +
  geom_histogram(bins = 20) +
  scale_x_continuous(breaks = 0:10, limits = c(0, 10)) +
  geom_vline(xintercept = mean(notes_ac24$total_ac[notes_ac24$total_ac > 0]), col = "blue") +
  geom_vline(xintercept = median(notes_ac24$total_ac[notes_ac24$total_ac > 0]), col = "green")

#2.3. Densitat
notes_ac24 |> 
  ggplot(aes(x = total_ac, fill = entregues)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = mean(notes_ac24$total_ac[notes_ac24$entregues == "Totes"]), col = "blue") +
  annotate("text", x = 0.5 + mean(notes_ac24$total_ac[notes_ac24$entregues == "Totes"]), y = 0.4, 
           label = round(mean(notes_ac24$total_ac[notes_ac24$entregues == "Totes"]), 1), col = "blue") +
  geom_vline(xintercept = mean(notes_ac24$total_ac[notes_ac24$entregues == "Alguna"]), col = "red") +
    annotate("text", x = 0.5 + mean(notes_ac24$total_ac[notes_ac24$entregues == "Alguna"]), y = 0.4, 
             label = round(mean(notes_ac24$total_ac[notes_ac24$entregues == "Alguna"]), 1), col = "red")

