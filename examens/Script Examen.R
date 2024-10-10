library(tidyverse)
library(janitor)
library(readxl)
notes24 <- read_csv("examens/Notes2024.csv") |> 
  clean_names()

notes24 |> 
  ggplot(aes(x = total_del_curs_real)) + 
  geom_histogram() +
  geom_vline(xintercept = mean(notes24$total_del_curs_real), col = "blue") +
  geom_vline(xintercept = median(notes24$total_del_curs_real), col = "green") +
  geom_vline(xintercept = quantile(notes24$total_del_curs_real, 0.9), col = "red") +
  theme_minimal()

notes24 |> 
  ggplot(aes(x = fct_reorder(numero_id,
                             total_del_curs_real), 
             y = total_del_curs_real)) + 
  geom_col(width = 1)

########## SEMINARIS 24 ###########
notes_sem <- bind_rows(
  read_xlsx("examens/Qualificacions_seminaris24.xlsx", sheet = 1) |> 
    transmute(nom = paste(Nom, Cognoms), id = `Número ID`, assist_sem = `A -seminaris`, nota_sem = `Total del curs (Real)`),
  read_xlsx("examens/Qualificacions_seminaris24.xlsx", sheet = 2) |> 
    transmute(nom = paste(Nom, Cognoms), id = `Número ID`, assist_sem = `A -seminaris`, nota_sem = `Total del curs (Real)`),
  read_xlsx("examens/Qualificacions_seminaris24.xlsx", sheet = 3) |> 
    transmute(nom = paste(Nom, Cognoms), id = `Número ID`, assist_sem = `A -seminaris`, nota_sem = `Total del curs (Real)`),
  read_xlsx("examens/Qualificacions_seminaris24.xlsx", sheet = 4) |> 
    transmute(nom = paste(Nom, Cognoms), id = `Número ID`, assist_sem = `A -seminaris`, nota_sem = `Total del curs (Real)`),
  read_xlsx("examens/Qualificacions_seminaris24.xlsx", sheet = 5) |> 
    transmute(nom = paste(Nom, Cognoms), id = `Número ID`, assist_sem = `A -seminaris`, nota_sem = `Total del curs (Real)`),
  read_xlsx("examens/Qualificacions_seminaris24.xlsx", sheet = 6) |> 
    transmute(nom = paste(Nom, Cognoms), id = `ID`, assist_sem = `A -seminaris`, nota_sem = `Total del curs (Real)`),
  read_xlsx("examens/Qualificacions_seminaris24.xlsx", sheet = 7) |> 
    transmute(nom = paste(Nom, Cognoms), id = `ID`, assist_sem = `A -seminaris`, nota_sem = `Total del curs (Real)`),
  read_xlsx("examens/Qualificacions_seminaris24.xlsx", sheet = 8) |> 
    transmute(nom = paste(Nom, Cognoms), id = `ID`, assist_sem = `A -seminaris`, nota_sem = `Total del curs (Real)`)
)

hist(notes_sem$nota_sem)


###### TEORIA 24 ############
presentats <- read_xlsx("examens/Presentats24.xlsx") |> 
  clean_names() |> 
  select(id = numero_id, pres = column1)

#Primer examen
notes_teoria <- read_csv("examens/notes_ac24r.csv") |> 
  transmute(nom = paste(Nom, Cognoms), id = `Número ID`, 
            teoria_ex_chr = `Total de Examen teoria 80% P1 (Real)`,
            teoria_ex_num = as.numeric(`Total de Examen teoria 80% P1 (Real)`),
            teoria_ex_rec = as.numeric(`Nota examen recuperació (Real)`),
            teoria_ac = `Total de Avaluació continua 20% (Real)`,
            teoria = `Total del curs (Real)`) |> 
  full_join(notes_sem, by = "id") |>
  full_join(presentats, by = "id") |>
  mutate(nota_sem = nota_sem / 10,
         nota_final1 = case_when(is.na(pres) ~ "NP",
                                 assist_sem < 3 ~ "NP",
                                 .default = as.character(round(teoria * 0.6 + nota_sem * 0.4, 1))),
         nota_acta = case_when(teoria_ex_num < 4 & as.numeric(nota_final1) >= 5 & nota_final1 != "NP" ~ "4.5", 
                               .default = nota_final1))

#Segon examen
notes_finals24 <- notes_teoria |>
  select(nom.x:teoria_ex_chr, teoria_ex_rec, teoria_ac, teoria, assist_sem, nota_sem, pres, nota_final1, nota_acta) |> 
  filter(nota_acta < 5) |>
  mutate(rec_sem = if_else(nota_sem < 5 & assist_sem > 2, "Y", "N"),
         rec_teo = if_else(teoria_ex_chr < 5, "Y", "N")) |> 
  mutate(nota_final2 = case_when(nota_final1 == "NP" ~ 999,
                                 rec_sem == "Y" & rec_teo == "Y" ~ round(teoria_ex_rec, 1),
                                 rec_sem == "Y" & rec_teo == "N" ~ teoria * 0.6 + teoria_ex_rec * 0.4,
                                 rec_sem == "N" & rec_teo == "Y" ~ round((teoria_ex_rec * 0.8 + teoria_ac * 0.2) * 0.6 + nota_sem * 0.4, 1))) |> 
  mutate(nota_final2 = if_else(teoria_ex_rec < 4 & nota_final2 >= 5, 4.5, nota_final2 ),) |> 
  transmute(nom = nom.x, id, nota_final2 = if_else(is.na(nota_final2), "NP", as.character(nota_final2)))
notes_finals24 |> 
  filter(id != "u212820") |> 
  write_csv("examens/notes24_final_r.csv")




#Generar notes recu
notes_teoria |> 
  filter(assist_sem < 3 | nota_final1 < 5 | teoria_ex_num < 4) |> 
  select(nom = nom.x, id, teoria_ex_chr:teoria, nota_sem, nota_final) |>
  mutate(rec = case_when(teoria_ex_num < 4 & nota_sem < 5 ~ "T-S",
                         teoria_ex_num >= 4 & nota_sem < 5 ~ "S",
                         .default = "T")) |>
  mutate(nota_rec = case_when(rec == "T-S" ~ teoria_ex_rec,
                              rec == "T" ~ (teoria_ex_rec * 0.8 + teoria_ac * 0.2) * 0.6 + nota_sem * 0.4,
                              rec == "S" ~ teoria * 0.6 + teoria_ex_rec * 0.4)) |> 
  View()
  write_csv("Notes_recu.csv")

  filter(!is.na(id)) |> 
  transmute(nom = nom.x, id, teoria_ex = teoria_ex_num, teoria_tot = teoria, 
            seminari = nota_sem, sem_assist = assist_sem, nota_final) |> 
  select(nom, id, nota_final) |> 
  write_csv("examens/notes24_final.csv")

notes_teoria |>
  mutate(nota_final = as.numeric(nota_final)) |> 
  summarize(no_presentats = sum(is.na(nota_final)),
            aprovats_n = sum(nota_final >= 5, na.rm = T),
            n = n(),
            aprovats_perc = sum(nota_final >= 5, na.rm = T) / n() * 100,
            mitjana = mean(nota_final, na.rm = T),
            mediana = median(nota_final, na.rm = T),
            max = max(nota_final, na.rm = T),
            min = min(nota_final, na.rm = T))


#Histogramets
hist(notes_teoria$teoria_ex_num, breaks = 10)
hist(as.numeric(notes_teoria$nota_final), breaks = 10)

#Descriptius
summary(as.numeric(notes_teoria$nota_final))
summary(as.numeric(notes_teoria$teoria_ex_num))


######## RECU
notes_teoria |> 
  filter(nota_final1 < 5)
