# Carreguem paquets i dades
library(dplyr)
library(readr)
library(ggplot2)
library(janitor)
theme_set(theme_minimal())

t1 <- read_csv("Notes/T1.csv") |> 
  clean_names() |>
  transmute(estudiant = paste(nom, cognoms),
            total_ac = total_de_avaluacio_continua_20_percent_real,
            moodle = total_de_test_moodle_real,
            dplyr = as.numeric(tasca_exercici_respondre_una_pregunta_amb_dplyr_real),
            ggplot = tasca_exercici_ggplot2_real,
            entregues = case_when(moodle > 0 & dplyr > 0 & ggplot > 0 ~ "Totes",
                              moodle > 0 | dplyr > 0 | ggplot > 0 ~ "Alguna",
                              TRUE ~ "Cap"))

t2 <- read_csv("Notes/T2.csv") |> 
  clean_names() |>
  transmute(estudiant = paste(nom, cognoms),
            total_ac = total_de_avaluacio_continua_20_percent_real,
            moodle = total_de_test_moodle_real,
            dplyr = as.numeric(tasca_exercici_respondre_una_pregunta_amb_dplyr_real),
            ggplot = tasca_exercici_ggplot2_real,
            entregues = case_when(moodle > 0 & dplyr > 0 & ggplot > 0 ~ "Totes",
                                  moodle > 0 | dplyr > 0 | ggplot > 0 ~ "Alguna",
                                  TRUE ~ "Cap"))

notes <- bind_rows(t1, t2, .id = "grup") |> 
  mutate(estudiant = "XXXX XXXX")

write_csv(notes, "data/notes.csv")

#1. ESTADÍSTICS DESCRIPTIUS

# 1.1. Inclòs 0
notes |> 
  group_by(grup) |> 
  summarize(mitjana = mean(total_ac),
            mediana = median(total_ac),
            maxim = max(total_ac),
            minim = min(total_ac),
            aprovats = sum(total_ac >= 5),
            perc_aprovats = mean(total_ac >= 5),
            zeros = sum(total_ac == 0),
            n = n())

#1.2. Exclòs 0
notes |> 
  filter(total_ac > 0) |> 
  group_by(grup) |> 
  summarize(mitjana = mean(total_ac),
            mediana = median(total_ac),
            maxim = max(total_ac),
            minim = min(total_ac),
            aprovats = sum(total_ac >= 5),
            perc_aprovats = mean(total_ac >= 5),
            zeros = sum(total_ac == 0),
            n = n())

#1.3. Els que han entregat les 3 activitats
notes |> 
  filter(entregues == "Totes") |> 
  group_by(grup) |> 
  summarize(mitjana = mean(total_ac),
            mediana = median(total_ac),
            maxim = max(total_ac),
            minim = min(total_ac),
            zeros = sum(total_ac == 0),
            n = n())


#2. VISUALITZACIONS

#2.1. Histograma
notes |> 
  ggplot(aes(x = total_ac)) +
  geom_histogram(bins = 20) +
  scale_x_continuous(breaks = 0:10) +
  geom_vline(xintercept = mean(notes$total_ac), col = "blue") +
  geom_vline(xintercept = median(notes$total_ac), col = "green")

#2.2. Histograma
notes |> 
  filter(total_ac > 0) |> 
  ggplot(aes(x = total_ac)) +
  geom_histogram(bins = 20) +
  scale_x_continuous(breaks = 0:10, limits = c(0, 10)) +
  geom_vline(xintercept = mean(notes$total_ac[notes$total_ac > 0]), col = "blue") +
  geom_vline(xintercept = median(notes$total_ac[notes$total_ac > 0]), col = "green")

#2.3. Densitat
notes |> 
  ggplot(aes(x = total_ac, fill = entregues)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = mean(notes$total_ac[notes$entregues == "Totes"]), col = "blue") +
  annotate("text", x = 0.5 + mean(notes$total_ac[notes$entregues == "Totes"]), y = 0.4, 
           label = round(mean(notes$total_ac[notes$entregues == "Totes"]), 1), col = "blue") +
  geom_vline(xintercept = mean(notes$total_ac[notes$entregues == "Alguna"]), col = "red") +
    annotate("text", x = 0.5 + mean(notes$total_ac[notes$entregues == "Alguna"]), y = 0.4, 
             label = round(mean(notes$total_ac[notes$entregues == "Alguna"]), 1), col = "red")

#2.4. Punts
notes |>
  filter(total_ac > 0) |> 
  ggplot(aes(x = grup, y = total_ac)) +
  geom_point(position = position_jitter(width = 0.1)) +
  scale_y_continuous(breaks = 0:10) +
  stat_summary(col = "red") +
  stat_summary(fun = "median", col = "green")
  
notes |>
  filter(total_ac > 0) |> 
  ggplot(aes(x = grup, y = total_ac)) +
  geom_point(position = position_jitter(width = 0.1),
             aes(col = entregues), alpha = 0.9) +
  scale_y_continuous(breaks = 0:10) +
  scale_color_manual(values = c("orange", "darkgreen")) +
  stat_summary(col = "darkred")





