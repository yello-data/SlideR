#####################################################################
# PART 3. EXERCISES
#####################################################################



# PREGUNTES

#0. Paquets
library(dplyr)
library(ggplot2)
library(forcats)

#1. A Euskadi, com està distribuït per municipis el vot a Bildu segons cada província?

elecc19 |>
  select(nombre_de_comunidad, nombre_de_provincia, 
         eh_bildu, votos_validos) |> 
  filter(nombre_de_comunidad == "País Vasco") |> 
  mutate(per_bildu = eh_bildu / votos_validos * 100) |> 
  ggplot(aes(x = per_bildu)) +
  geom_histogram(fill = "lightcyan4") +
  facet_wrap(nombre_de_provincia~.) +
  theme_minimal() +
  labs(x = "Percentatge vots", y = "Recompte",
       title = "Percentatge de vots a Bildu per municipi segons província")



#2. En quants municipis de cada provincia d'Espanya, els vots al PACMA van superar el 2% dels vots?

elecc19 |>
  select(nombre_de_comunidad, nombre_de_provincia,
         total_votantes:votos_nulos, pacma) |> 
  mutate(per_pacma = pacma / votos_validos * 100,
         pacma_2 = per_pacma > 2) |>
  group_by(nombre_de_comunidad) |> 
  summarize(pacma = sum(pacma_2)) |> 
  ggplot(aes(x = fct_reorder(nombre_de_comunidad, pacma), y = pacma)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(x = "Comunitat Autònoma", y = "Nombre de municipis",
       title = "Municipis amb vot al PACMA superior al 2%")



#3. En quin municipi de cada CCAA hi ha hagut menys abstenció?

elecc19 |>
  select(ccaa = nombre_de_comunidad, mun = nombre_de_municipio, votos_validos, total_censo_electoral) |> 
  mutate(abs = round(votos_validos / total_censo_electoral * 100, 1)) |> 
  group_by(ccaa) |> 
  filter(abs == min(abs)) |> 
  mutate(nom = paste0(mun, " (", ccaa, ")")) |> 
  ggplot(aes(x = fct_reorder(nom, abs), y = abs)) +
  geom_point(size = 7.6, col = "darkblue") +
  geom_text(aes(label = abs), col = "white", size = 2.8) +
  coord_flip() +
  theme_minimal() +
  labs(y = "% abstenció", x = "Municipi",
       title = "Municipi amb menys abstenció de cada Comunitat Autònoma")
 


