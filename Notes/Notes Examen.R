# Carreguem paquets i dades
library(dplyr)
library(readr)
library(ggplot2)
library(janitor)
library(tidyr)
theme_set(theme_minimal())

t1 <- read_csv("Notes/Examen MQI1P.csv") |> 
  clean_names() |>
  separate(cognoms, into = c("cognoms", "2n"), sep = " ") |> 
  transmute(estudiant = paste(nom, cognoms),
            numero_id,
            total_ac = total_de_avaluacio_continua_20_percent_real,
            moodle = total_de_test_moodle_real,
            dplyr = as.numeric(tasca_exercici_respondre_una_pregunta_amb_dplyr_real),
            ggplot = tasca_exercici_ggplot2_real,
            entregues = case_when(moodle > 0 & dplyr > 0 & ggplot > 0 ~ "Totes",
                                  moodle > 0 | dplyr > 0 | ggplot > 0 ~ "Alguna",
                                  TRUE ~ "Cap"),
            examen = as.numeric(total_de_examen_teoria_80_percent_real),
            nota_teoria = total_del_curs_real)

t2 <- read_csv("Notes/Examen MQI2P.csv") |> 
  clean_names() |>
  separate(cognoms, into = c("cognoms", "2n"), sep = " ") |> 
  transmute(estudiant = paste(nom, cognoms),
            numero_id,
            total_ac = total_de_avaluacio_continua_20_percent_real,
            moodle = total_de_test_moodle_real,
            dplyr = as.numeric(tasca_exercici_respondre_una_pregunta_amb_dplyr_real),
            ggplot = tasca_exercici_ggplot2_real,
            entregues = case_when(moodle > 0 & dplyr > 0 & ggplot > 0 ~ "Totes",
                                  moodle > 0 | dplyr > 0 | ggplot > 0 ~ "Alguna",
                                  TRUE ~ "Cap"),
            examen = as.numeric(total_de_examen_teoria_80_percent_real),
            nota_teoria = total_del_curs_real)

notes <- bind_rows(t1, t2, .id = "grup") |> 
  mutate(nota_teoria_num = if_else(is.na(examen), 0, nota_teoria),
         nota_teoria_cat = if_else(is.na(examen), "NP", as.character(nota_teoria)))

write_csv(notes, "Notes/notes_examen.csv")

#1. ESTADÍSTICS DESCRIPTIUS

notes <- read_csv("data/notes.csv")


View(notes)
# 1.1. EXAMEN
notes |> 
  group_by(grup) |> 
  summarize(mitjana = mean(examen, na.rm = T),
            mediana = median(examen, na.rm = T),
            maxim = max(examen, na.rm = T),
            minim = min(examen, na.rm = T),
            aprovats = sum(examen >= 5, na.rm = T),
            sobre_4 = sum(examen >= 4, na.rm = T),
            perc_aprovats = mean(examen >= 5, na.rm = T),
            perc_sobre_4 = mean(examen >= 4, na.rm = T),
            n = n())


#1.3. NOTA FINAL
notes |> 
  group_by(grup) |> 
  summarize(mitjana = mean(nota_teoria, na.rm = T),
            mediana = median(nota_teoria, na.rm = T),
            maxim = max(nota_teoria, na.rm = T),
            minim = min(nota_teoria, na.rm = T),
            aprovats = sum(nota_teoria >= 5, na.rm = T),
            perc_aprovats = mean(nota_teoria >= 5, na.rm = T),
            n = n())


#2. VISUALITZACIONS

#2.1. Histograma
notes |> 
  ggplot(aes(x = examen)) +
  geom_histogram(bins = 20) +
  scale_x_continuous(breaks = 0:10) +
  geom_vline(xintercept = mean(notes$total_ac), col = "blue") +
  geom_vline(xintercept = median(notes$total_ac), col = "green")

#2.3. Densitat
notes |> 
  ggplot(aes(x = examen, fill = entregues)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = mean(notes$total_teoria[notes$entregues == "Totes"]), col = "blue") +
  annotate("text", x = 0.5 + mean(notes$total_teoria[notes$entregues == "Totes"]), y = 0.4, 
           label = round(mean(notes$total_teoria[notes$entregues == "Totes"]), 1), col = "blue") +
  geom_vline(xintercept = mean(notes$total_teoria[notes$entregues == "Alguna"]), col = "red") +
  annotate("text", x = 0.5 + mean(notes$total_teoria[notes$entregues == "Alguna"]), y = 0.4, 
           label = round(mean(notes$total_teoria[notes$entregues == "Alguna"]), 1), col = "red")

#2.4. Punts
notes |>
  ggplot(aes(x = grup, y = nota_teoria)) +
  geom_point(position = position_jitter(width = 0.1)) +
  scale_y_continuous(breaks = 0:10) +
  stat_summary(col = "red") +
  stat_summary(fun = "median", col = "green")

notes |>
  ggplot(aes(x = grup, y = nota_teoria)) +
  geom_point(position = position_jitter(width = 0.1),
             aes(col = entregues), alpha = 0.9) +
  scale_y_continuous(breaks = 0:10) +
  scale_color_manual(values = c("orange", "red", "darkgreen")) +
  stat_summary(col = "darkred")

notes |>
  ggplot(aes(x = total_ac, y = examen)) +
  geom_text(aes(label = estudiant), size = 2.6) +
  geom_smooth(method = "lm", se = F)

cor(notes$examen, notes$total_ac, use = "complete.obs")
  




