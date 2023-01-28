#1. Dades obertes: https://analisi.transparenciacatalunya.cat/ca/browse?q=contractaci%C3%B3%20menor
#1.1. Contractació de la Generalitat de Catalunya: menorsEconomia. https://analisi.transparenciacatalunya.cat/Economia/Contractaci-de-la-Generalitat-de-Catalunya-menors/qjue-2pk9

#Ley 9/2017, de 8 de noviembre, de Contratos del Sector Público
#https://www.boe.es/buscar/act.php?id=BOE-A-2017-12902&b=161&tn=1&p=20171109#a1-28
##Artículo 118. Expediente de contratación en contratos menores.
# Entrada en vigor del texto original: 09/03/2018
# Entrada en vigor del texto original: 06/02/2020


#1. PREPARATION

##1.1. Packages
library(pacman)
p_load(tidyverse, lubridate, janitor, Hmisc)
theme_set(theme_minimal())

##1.2. Functions
fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}

##1.3. Download data
### Go to: Contractació de Catalunya: https://analisi.transparenciacatalunya.cat/Sector-P-blic/Contractaci-de-Catalunya/hb6v-jcbf
### Exporta -> CSV -> name: 'catalunya_all.csv'
### Save to ../Data

catc <- read_csv("Data/catalunya_all.csv") |> 
  clean_names()

# Crear categòrica amb l'import i netejar dades (poques abans 2017)
catc_16_22 <- catc |> 
  filter(between(exercici, 2016, 2022)) |> 
  mutate(import_cat = fct_case_when(import_adjudicacio < 10000 ~ "Menys 10000",
                            import_adjudicacio < 14500 ~ "10000-14499",
                            import_adjudicacio < 15000 ~ "14500-14999",
                            import_adjudicacio < 18000 ~ "15000-17999",
                            import_adjudicacio < 20000 ~ "18000-20000",
                            TRUE ~ "Més 20000"),
         data = dmy(data_adjudicacio)) |> 
  arrange(subjecte_ambit, organisme_contractant) |> 
  select(situacio_contractual, procediment_adjudicacio, exercici, data, 
         subjecte_ambit, agrupacio_organisme, organisme_contractant,
         tipus_contracte, adjudicatari, import_adjudicacio, import_cat, descripcio_expedient)


# 2. UNIVARIANT

##2.1. DADES PER CONTRACTE

#2.1.1. Contractes (N) per tipus i any per franja d'import d'adjudicació
catc_16_22 |> 
  count(exercici, import_cat) |> 
  ggplot(aes(x = exercici, y = n, fill = import_cat)) +
  geom_col() +
  scale_fill_manual(values = c("gray80", "gray75", "red", "gray65", 
                               "gray60", "gray55"))

#2.1.2. Quantitat de contractes (%) per franja d'import d'adjudicació  
catc_16_22 |> 
  count(exercici, import_cat) |> 
  ggplot(aes(x = exercici, y = n, fill = import_cat)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = c("gray80", "gray75", "red", "gray65", 
                               "gray60", "gray55"))
  

## 2.2. DADES PER ORGANISME CONTRACTANT

# Canviem unitat observació per organisme contractant
# Variables noves:
## -Número de contractes 14500-14999 (n)
## -Percentatge contractes 14500-14999 sobre el total de contractes (n_perc)
## -Percentatge import sobre el total import (import_perc)

catcm_16_22 <- catc_16_22 |> 
  group_by(exercici, organisme_contractant, import_cat) |> 
  summarize(import = sum(import_adjudicacio),
            n = n()) |> 
  group_by(exercici, organisme_contractant) |> 
  mutate(n_perc = (n / sum(n)) * 100,
         import_perc = (import / sum(import)) * 100) |>
  filter(import_cat == "14500-14999")

# 2.2.1. Hall of fame per organisme contractant
## Eliminem imports petits (suma de tots inferior a 30m o inferior a 4N)
hallfame <- catcm_16_22 |> 
  filter(import > 30000, n > 4)

# A. Per nombre de contractes
head(arrange(hallfame, desc(n)), 12)

# B. Per percentatge de contractes
head(arrange(hallfame, desc(n_perc)), 12)

# C. Per percentatge d'import
head(arrange(hallfame, desc(import_perc)), 12)

# 2.2.2. Han augmentat els contractes de 14.9k? Dades per N i per import
hallfame |> 
  ggplot(aes(x = exercici)) +
  geom_smooth(aes(y = import_perc), method = "lm") +
  geom_smooth(aes(y = n_perc), method = "lm", col = "red")


## 2.2. DADES PER ADJUDICATARI

# Canvi unitat observació per adjudicatari
## -Número de contractes 14500-14999 (n)
## -Percentatge contractes 14500-14999 sobre el total de contractes (n_perc)
## -Percentatge import sobre el total import (import_perc)
catcc_16_22 <- catc_16_22 |> 
  group_by(exercici, adjudicatari, import_cat) |> 
  summarize(import = sum(import_adjudicacio),
            n = n()) |> 
  group_by(exercici, adjudicatari) |> 
  mutate(n_perc = (n / sum(n)) * 100,
         import_perc = (import / sum(import)) * 100) |>
  filter(import_cat == "14500-14999")

# 2.3.1. Hall of fame per adjudicatari
## Eliminem imports petits (suma de tots inferior a 30m o inferior a 4N)
hallfame_adj <- catcc_16_22 |>
  filter(import > 30000, n > 4)

# A. Per nombre de contractes
head(arrange(hallfame_adj, desc(n)), 12)
# B. Per percentatge de contractes
head(arrange(hallfame_adj, desc(n_perc)), 12)
# C. Per percentatge d'import
head(arrange(hallfame_adj, desc(import_perc)), 12)

# 2.3.2. Han augmentat els contractes de 14.9k? Dades per N i per import
hallfame_adj |> 
  ggplot(aes(x = exercici)) +
  geom_smooth(aes(y = import_perc), method = "lm") +
  geom_smooth(aes(y = n_perc), method = "lm", col = "red")





############### DIFERÈNCIES ##################

difplot <- function(df, break.point){
  df |> 
    ggplot(aes(x = data, y = import_adjudicacio)) +
    geom_point(alpha = 0.2) +
    geom_vline(xintercept = as.Date(break.point), lty = 2, col = "blue") +
    stat_summary(fun.data = "mean_cl_normal", col = "red", size = 0.2)  
}

catc_diffs18 <- catc_16_22 |> 
  filter(procediment_adjudicacio == "Menor") |> 
  filter(data > "2018-02-09" & data < "2018-04-09") |> 
  filter(import_adjudicacio < 50000)

catc_diffs20 <- catc_16_22 |> 
  filter(procediment_adjudicacio == "Menor") |> 
  filter(data > "2020-01-06" & data < "2020-03-06") |> 
  filter(import_adjudicacio < 50000)


#1. Vista general del N de contractes per setmanes
catc_16_22 |> 
  filter(procediment_adjudicacio == "Menor") |> 
  mutate(week = strftime(data)) |> 
  ggplot(aes(x = week, fill = import_cat)) +
  geom_bar()

#2. Entrada en vigor Llei 9/2017 (9 de març 2018)

## A) Variació en el N de contractes menors en l'entrada en vigor
catc_diffs18 |> 
  ggplot(aes(x = data, fill = import_cat)) +
  geom_bar() +
  geom_vline(xintercept = as.Date("2018-03-09"), lty = 2, col = "blue")

## B) Variació en el l'import d'adjudicació de contractes menors en l'entrada en vigor
difplot(df = catc_diffs18, break.point = "2018-03-09")

#2. Entrada en vigor última actualització Llei 9/2017 (6 de març 2020)

## B) Variació en el l'import d'adjudicació de contractes menors en l'entrada en vigor de l'actualització
difplot(df = catc_diffs20, break.point = "2020-02-06")


  
