library(tidyverse)
notes_examen <- read_csv("Notes/notes_examen.csv")

notes_full <- full_join(
  notes_examen |> 
    select(grup:numero_id, total_ac, examen, nota_teoria_num),
  prac |> 
    transmute(IDUSUARI = str_to_lower(IDUSUARI), nota_practica = `Nota publicable`),
  by = c("numero_id" = "IDUSUARI")
) |> 
  mutate(nota_practica = if_else(is.na(nota_practica), 0, nota_practica),
         nota_practica = if_else(numero_id == "u212793", 6.6, nota_practica),
         nota_final = round(nota_teoria_num * 0.6 + nota_practica * 0.4, 1),
         aprovat = if_else(examen >= 4 & nota_final >= 5, "S", "N")) |> 
  filter(numero_id != "u216371")



notes_full |>
  mutate(nota_acta = if_else(aprovat == "N" & nota_final >= 5, 4.5, nota_final),
         nota_acta = if_else(examen == "NA", "NP", as.character(nota_acta))) |> 
  arrange(desc(aprovat), desc(nota_acta)) |> 
  select(grup, estudiant, numero_id, teoria_examen = examen, teoria_total = nota_teoria_num, practica_total = nota_practica, nota_final = nota_acta) |> 
  write_csv("notes_finals.csv")



notes_full |> 
  ggplot(aes(x = nota_final)) +
  geom_histogram(bins = 20)



notes_finals |> 
  mutate(final = teoria_total * 0.6 + practica_total * 0.4) |> 
  arrange(desc(final))



